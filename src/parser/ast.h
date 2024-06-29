// Copyright 2024 Lucas Norman

#pragma once

#include <iostream>
#include <memory>
#include <ranges>
#include <vector>
#include <map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"

#include "../diagnostics/generator.h"

// to keep track of the variables that are available in the current scope/function when generating LLVM IR. Each map in the vector is a scope
static std::vector<std::map<std::string, llvm::AllocaInst*>> scopeStack;

// helper function to find a variable in the scopeStack
static llvm::AllocaInst *getAllocaInst(const std::string& identifier) {
    // go over each scope and check if the variable is defined (reverse the scopeStack to check the most nested scopes first)
    for (auto& scope : std::ranges::reverse_view(scopeStack)) {
        auto it = scope.find(identifier);
        if (it != scope.end()) return it->second;
    }
    return nullptr;
}

// helper function to set a variable in the scopeStack
static void setAllocaInst(const std::string& identifier, llvm::AllocaInst* allocaInst) {
    if (scopeStack.empty()) scopeStack.emplace_back();
    scopeStack.back()[identifier] = allocaInst;
}

// helper function to get the llvm::Type* from a type name/string
static llvm::Type *getLLVMType(const std::string& type, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (type == "bool") {
        return builder->getInt1Ty();
    } else if (type == "i8") {
        return builder->getInt8Ty();
    } else if (type == "i16") {
        return builder->getInt16Ty();
    } else if (type == "i32") {
        return builder->getInt32Ty();
    } else if (type == "i64") {
        return builder->getInt64Ty();
    } else if (type == "f32") {
        return builder->getFloatTy();
    } else if (type == "f64") {
        return builder->getDoubleTy();
    } else if (type == "char") {
        return builder->getInt8Ty();
    } else if (type == "str") {
        return llvm::PointerType::get(builder->getInt8Ty(), 0);
    } else {
        // return a void type
        return builder->getVoidTy();
    }
}

// helper function to get the llvm::Value* from an integer (since it might vary in bit-width)
static llvm::Value *getIntegerValue(uint64_t number, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (number <= 2147483647) {
        // limit for signed i32
        return builder->getInt32(number);
    } else if (number <= 9223372036854775807) {
        // limit for signed i64
        return builder->getInt64(number);
    } else {
        generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid integer literal", "The integer is to large to be represented as an integer");
        return nullptr;
    }
}

// helper function to create a llvm cast instruction
static llvm::Value *createCast(llvm::Value *value, llvm::Type *type, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    llvm::Type *srcType = value->getType();

    if (srcType == type) {
        // no cast needed
        return value;
    }

    // if the source type is bool
    if (srcType->isIntegerTy(1)) {
        if (type->isIntegerTy()) {
            return builder->CreateCast(llvm::Instruction::ZExt, value, type, "tmpcast");
        } else if (type->isFloatingPointTy()) {
            return builder->CreateCast(llvm::Instruction::UIToFP, value, type, "tmpcast");
        }
    }

    // if destination type is bool, then check if the value does not equal 0
    if (type->isIntegerTy(1)) {
        if (srcType->isIntegerTy()) {
            return builder->CreateICmpNE(value, llvm::ConstantInt::get(srcType, 0), "cmptozero");
        } else if (srcType->isFloatingPointTy()) {
            return builder->CreateFCmpONE(value, llvm::ConstantFP::get(srcType, 0), "cmptozero");
        }
    }

    // if the src type is int or float and type is int or float
    if ((srcType->isIntegerTy() || srcType->isFloatingPointTy()) && (type->isIntegerTy() || type->isFloatingPointTy())) {
        llvm::CastInst::CastOps castOperation = llvm::CastInst::getCastOpcode(value, true, type, true);
        return builder->CreateCast(castOperation, value, type, "tmpcast");
    }

    // throw error, cast is not supported
    std::string stringSrcType;
    std::string stringType;
    llvm::raw_string_ostream stream1(stringSrcType);
    llvm::raw_string_ostream stream2(stringType);
    srcType->print(stream1);
    type->print(stream2);
    generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid cast", "Cannot cast from '" + stringSrcType + "' to '" + stringType + "'");
    return nullptr;
}

// helper function for getting the boolean representation of a llvm::Value
static llvm::Value *getBooleanValue(llvm::Value *value, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    return createCast(value, builder->getInt1Ty(), builder);
}

// helper function to create an alloca instruction in the entry block of a function. Used with mutable variables
static llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *fn, const std::string& variableName, llvm::Type *type) {
    llvm::IRBuilder<> temporaryBuilder(&fn->getEntryBlock(), fn->getEntryBlock().begin());
    return temporaryBuilder.CreateAlloca(type, nullptr, variableName);
}

// helper function to create multiplication
static llvm::Value *createBinaryOperation(llvm::Value *leftValue, llvm::Value *rightValue, const std::string& operation, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    bool isFloatingPointOperation = leftValue->getType()->isFloatingPointTy();
    // if it is an integer that is not an i1 (boolean)
    bool isIntegerOperation = leftValue->getType()->isIntegerTy() && !leftValue->getType()->isIntegerTy(1);

    // these can be performed with different types because both sides are cast to booleans
    if (operation == "&&") {
        return builder->CreateAnd(getBooleanValue(leftValue, builder), getBooleanValue(rightValue, builder), "andtmp");
    } else if (operation == "||") {
        return builder->CreateOr(getBooleanValue(leftValue, builder), getBooleanValue(rightValue, builder), "ortmp");
    }

    // check if the left and right expression have the same type
    if (leftValue->getType() != rightValue->getType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(), "Type mismatch in binary operation", "The left and right hand sides of the binary operator '" + operation + "' have different types");
        return nullptr;
    }

    // these operations can only be performed if the types are the same
    if (operation == "+") {
        if (isFloatingPointOperation) {
            return builder->CreateFAdd(leftValue, rightValue, "addfloattmp");
        } else if (isIntegerOperation) {
            return builder->CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (operation == "-") {
        if (isFloatingPointOperation) {
            return builder->CreateFSub(leftValue, rightValue, "subfloattmp");
        } else if (isIntegerOperation) {
            return builder->CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (operation == "*") {
        if (isFloatingPointOperation) {
            return builder->CreateFMul(leftValue, rightValue, "mulfloattmp");
        } else if (isIntegerOperation) {
            return builder->CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (operation == "/") {
        if (isFloatingPointOperation) {
            return builder->CreateFDiv(leftValue, rightValue, "divfloattmp");
        } else if (isIntegerOperation) {
            return builder->CreateSDiv(leftValue, rightValue, "divtmp");
        }
    } else if (operation == "%") {
        if (isFloatingPointOperation) {
            return builder->CreateFRem(leftValue, rightValue, "remfloattmp");
        } else if (isIntegerOperation) {
            return builder->CreateSRem(leftValue, rightValue, "remtmp");
        }
    } else if (operation == "==") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOEQ(leftValue, rightValue, "cmpfloattmpequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpEQ(leftValue, rightValue, "cmptmpequals");
        }
    } else if (operation == "!=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpONE(leftValue, rightValue, "cmpfloattmpnotequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpNE(leftValue, rightValue, "cmptmpnotequals");
        }
    } else if (operation == "<") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOLT(leftValue, rightValue, "cmpfloattmpless");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSLT(leftValue, rightValue, "cmptmpless");
        }
    } else if (operation == ">") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOGT(leftValue, rightValue, "cmpfloattmpgreater");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSGT(leftValue, rightValue, "cmptmpgreater");
        }
    } else if (operation == "<=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOLE(leftValue, rightValue, "cmpfloattmplessequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSLE(leftValue, rightValue, "cmptmplessequals");
        }
    } else if (operation == ">=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOGE(leftValue, rightValue, "cmpfloattmpgreaterequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSGE(leftValue, rightValue, "cmptmpgreaterequals");
        }
    }

    // if no operators matched, then throw an error
    std::string stringType;
    llvm::raw_string_ostream stream(stringType);
    leftValue->getType()->print(stream);
    generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid binary operator", "The binary operator '" + operation + "' is not supported with the type '" + stringType + "'");
    return nullptr;
}

enum CodegenResultType {
    VALUE_CODEGEN_RESULT,
    PARAM_CODEGEN_RESULT,
    FUNCTION_CODEGEN_RESULT
};

// type to store parameter result in CodegenResult, since it has two fields
struct ParamCodegenResult {
    std::string identifier;
    llvm::Type *type;

    ParamCodegenResult(std::string identifier, llvm::Type *type): identifier(identifier), type(type) {}
    ~ParamCodegenResult() {}
};

// type to return from codegen methods, to handle multiple return types like llvm::Value* and llvm::Function*
struct CodegenResult {
    union {
        llvm::Value *value;
        ParamCodegenResult param;
        llvm::Function *fn;
    };
    CodegenResultType resultType;

    CodegenResult(llvm::Value *value, CodegenResultType resultType): value(value), resultType(resultType) {}
    CodegenResult(ParamCodegenResult param, CodegenResultType resultType): param(param), resultType(resultType) {}
    CodegenResult(llvm::Function *fn, CodegenResultType resultType): fn(fn), resultType(resultType) {}

    ~CodegenResult() {}
};

class ASTNode {
 public:
    virtual ~ASTNode() {}

    virtual void print(int depth) const = 0;
    virtual std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const = 0;
};

class AST {
    std::vector<ASTNode*> rootNodes;

 public:
    explicit AST(std::vector<ASTNode*> rootNodes): rootNodes(rootNodes) {}
    ~AST() {
        // delete each node the vector
        for (ASTNode *node : rootNodes) {
            delete node;
        }
    }

    void print() const {
        // print each node the vector
        for (ASTNode *node : rootNodes) {
            node->print(0);
        }
    }

    void codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const {
        // codegen each node the vector
        for (ASTNode *node : rootNodes) {
            node->codegen(ctx, builder, moduleLLVM);
        }
    }
};

// to keep track of all loop information when generating break and continue statements
struct LoopContext {
    ASTNode *updateStatement;
    llvm::BasicBlock *conditionalBlock;
    llvm::BasicBlock *mergeBlock;

    inline bool isForLoop() const { return updateStatement != nullptr; }
};

// to keep track of information for the current loops, so that break and continue statements can be generated correctly
static std::vector<LoopContext> loopContexts;

class ASTVariableExpression : public ASTNode {
    std::string identifier;

 public:
    explicit ASTVariableExpression(std::string identifier): identifier(identifier) {}
    ~ASTVariableExpression() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Expression: " << identifier << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        llvm::AllocaInst *allocaInst = getAllocaInst(identifier);
        if (allocaInst == nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Unknown variable name", "The variable '" + identifier + "' could not be found");
            return nullptr;
        }
        return std::make_unique<CodegenResult>(builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst, identifier.c_str()), VALUE_CODEGEN_RESULT);
    }
};

class ASTInteger : public ASTNode {
    uint64_t number;

 public:
    explicit ASTInteger(uint64_t number): number(number) {}
    ~ASTInteger() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Integer: " << number << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(getIntegerValue(number, builder), VALUE_CODEGEN_RESULT);
    }
};

class ASTBool : public ASTNode {
    bool value;

 public:
    explicit ASTBool(bool value): value(value) {}
    ~ASTBool() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Boolean: " << value << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(builder->getInt1(value), VALUE_CODEGEN_RESULT);
    }
};

class ASTFloat : public ASTNode {
    double number;

 public:
    explicit ASTFloat(double number): number(number) {}
    ~ASTFloat() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Float: " << number << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(llvm::ConstantFP::get(builder->getDoubleTy(), number), VALUE_CODEGEN_RESULT);
    }
};

class ASTString : public ASTNode {
    std::string text;

 public:
    explicit ASTString(std::string text): text(text) {}
    ~ASTString() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "String: " << text << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(builder->CreateGlobalStringPtr(text, "strlit"), VALUE_CODEGEN_RESULT);
    }
};

class ASTChar : public ASTNode {
    char character;

 public:
    explicit ASTChar(char character): character(character) {}
    ~ASTChar() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Character: " << character << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(builder->getInt8(character), VALUE_CODEGEN_RESULT);
    }
};

class ASTTypeCast : public ASTNode {
    ASTNode *expression;
    std::string type;

 public:
    explicit ASTTypeCast(ASTNode *expression, std::string type): expression(expression), type(type) {}
    ~ASTTypeCast() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Type Cast: " << type << '\n';
        expression->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        // check if the expression result is valid
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in type cast operator", "The expression in the type cast has an invalid value");
        }
        return std::make_unique<CodegenResult>(createCast(expressionResult->value, getLLVMType(type, builder), builder), VALUE_CODEGEN_RESULT);
    }
};

// this could also be called a scope
class ASTCompoundStatement : public ASTNode {
    std::vector<ASTNode*> statementList;

 public:
    explicit ASTCompoundStatement(std::vector<ASTNode*> statementList): statementList(statementList) {}
    // also add an empty constructor for no statements
    ASTCompoundStatement() {}
    ~ASTCompoundStatement() {
        // delete each node the vector
        for (ASTNode *statement : statementList) {
            delete statement;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Compound Statement:\n";
        // print each node the vector
        for (ASTNode *statement : statementList) {
            statement->print(depth + 1);
        }
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // start a new scope
        scopeStack.emplace_back();

        // codegen each node in the vector
        for (ASTNode *statement : statementList) {
            statement->codegen(ctx, builder, moduleLLVM);
            // if the latest basic block has a terminal statement, then skip generating the rest
            if (builder->GetInsertBlock()->getTerminator()) break;
        }

        // end the scope
        scopeStack.pop_back();
        return nullptr;
    }
};

class ASTBinaryOperator : public ASTNode {
    ASTNode *left;
    ASTNode *right;
    std::string operation;

 public:
    ASTBinaryOperator(ASTNode *left, ASTNode *right, std::string operation): left(left), right(right), operation(operation) {}
    ~ASTBinaryOperator() {
        delete left;
        delete right;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Binary Operator: " << operation << '\n';
        left->print(depth + 1);
        right->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> leftResult = left->codegen(ctx, builder, moduleLLVM);
        std::unique_ptr<CodegenResult> rightResult = right->codegen(ctx, builder, moduleLLVM);
        // check if both the left and right results are valid
        if (leftResult == nullptr || leftResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid left hand side of binary operator", "The left hand side of the binary operator '" + operation + "' has an invalid value");
            return nullptr;
        }
        if (rightResult == nullptr || rightResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid right hand side of binary operator", "The right hand side of the binary operator '" + operation + "' has an invalid value");
            return nullptr;
        }
        return std::make_unique<CodegenResult>(createBinaryOperation(leftResult->value, rightResult->value, operation, builder), VALUE_CODEGEN_RESULT);
    }
};

class ASTUnaryOperator : public ASTNode {
    ASTNode *expression;
    std::string operation;

 public:
    ASTUnaryOperator(ASTNode *expression, std::string operation): expression(expression), operation(operation) {}
    ~ASTUnaryOperator() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Unary Operator: " << operation << '\n';
        expression->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        // check if the expression result is valid
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in unary operator", "The expression in the unary operator '" + operation + "' has an invalid value");
        }
        bool isFloatingPointOperation = expressionResult->value->getType()->isFloatingPointTy();
        bool isIntegerOperation = expressionResult->value->getType()->isIntegerTy();
        llvm::Value *resultValue = nullptr;
        if (operation == "!") {
            resultValue = builder->CreateNot(getBooleanValue(expressionResult->value, builder), "nottmp");
        } else if (operation == "-") {
            if (isFloatingPointOperation) {
                resultValue = builder->CreateFNeg(expressionResult->value, "negtmp");
            } else if (isIntegerOperation) {
                resultValue = builder->CreateNeg(expressionResult->value, "negtmp");
            }
        } else if (operation == "+") {
            // does not change the value (but check if it is used with valid types anyway)
            if (isFloatingPointOperation || isIntegerOperation) resultValue = expressionResult->value;
        }
        // check if the resultValue is a nullptr, then throw an error
        if (!resultValue) {
            std::string stringType;
            llvm::raw_string_ostream stream(stringType);
            expressionResult->value->getType()->print(stream);
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid unary operator", "The unary operator '" + operation + "' is not supported with the type '" + stringType + "'");
            return nullptr;
        }
        return std::make_unique<CodegenResult>(resultValue, VALUE_CODEGEN_RESULT);
    }
};

class ASTIncrementDecrementOperator : public ASTNode {
    std::string identifier;
    std::string operation;

 public:
    explicit ASTIncrementDecrementOperator(std::string identifier, std::string operation): identifier(identifier), operation(operation) {}
    ~ASTIncrementDecrementOperator() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Increment/Decrement Operator:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Operator Type: " << operation << "\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // get the alloca instance
        llvm::AllocaInst *allocaInstance = getAllocaInst(identifier);
        if (allocaInstance == nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Unknown variable name", "The variable '" + identifier + "' could not be found");
            return nullptr;
        }

        // load the value
        llvm::Value *loadedValue = builder->CreateLoad(allocaInstance->getAllocatedType(), allocaInstance, identifier.c_str());

        llvm::Type *loadedValueType = loadedValue->getType();
        bool isIntegerType = loadedValueType->isIntegerTy();
        bool isFloatingPointType = loadedValueType->isFloatingPointTy();
        if (!isIntegerType && !isFloatingPointType) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid use of increment/decrement operator", "The variable '" + identifier + "' is not an integer or floating point value");
            return nullptr;
        }

        // to store the resulting value of the operation
        llvm::Value *resultValue;
        if (operation == "x++") {
            // set the resultValue to loaded value
            resultValue = loadedValue;
            // increment the value by one and store it
            builder->CreateStore(
                isIntegerType ? builder->CreateAdd(loadedValue, llvm::ConstantInt::get(loadedValueType, 1), "incrementtmp") : builder->CreateFAdd(loadedValue, llvm::ConstantFP::get(loadedValueType, 1), "incrementfloattmp"),
                allocaInstance);
        } else if (operation == "x--") {
            // set the resultValue to loaded value
            resultValue = loadedValue;
            // decrement the value by one and store it
            builder->CreateStore(
                isIntegerType ? builder->CreateSub(loadedValue, llvm::ConstantInt::get(loadedValueType, 1), "decrementtmp") : builder->CreateFSub(loadedValue, llvm::ConstantFP::get(loadedValueType, 1), "decrementfloattmp"),
                allocaInstance);
        } else if (operation == "++x") {
            // increment the value by one and set it to resultValue
            resultValue = isIntegerType ? builder->CreateAdd(loadedValue, llvm::ConstantInt::get(loadedValueType, 1), "incrementtmp") : builder->CreateFAdd(loadedValue, llvm::ConstantFP::get(loadedValueType, 1), "incrementfloattmp");
            // store the result value
            builder->CreateStore(resultValue, allocaInstance);
        } else if (operation == "--x") {
            // decrement the value by one and set it to resultValue
            resultValue = isIntegerType ? builder->CreateSub(loadedValue, llvm::ConstantInt::get(loadedValueType, 1), "decrementtmp") : builder->CreateFSub(loadedValue, llvm::ConstantFP::get(loadedValueType, 1), "decrementfloattmp");
            // store the result value
            builder->CreateStore(resultValue, allocaInstance);
        } else {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid increment/decrement operator", "The operator '" + operation + "' is not supported");
            return nullptr;
        }
        return std::make_unique<CodegenResult>(resultValue, VALUE_CODEGEN_RESULT);
    }
};

class ASTParameter : public ASTNode {
    std::string identifier;
    std::string type;

 public:
    ASTParameter(std::string identifier, std::string type): identifier(identifier), type(type) {}
    ~ASTParameter() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Parameter:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Type: " << type << "\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(ParamCodegenResult(identifier, getLLVMType(type, builder)), PARAM_CODEGEN_RESULT);
    }
};

class ASTFunctionPrototype : public ASTNode {
    std::string identifier;
    std::vector<ASTNode*> parameterList;
    std::string returnType;

 public:
    ASTFunctionPrototype(std::string identifier, std::vector<ASTNode*> parameterList, std::string returnType): identifier(identifier), parameterList(parameterList), returnType(returnType) {}
    ~ASTFunctionPrototype() {
        // delete each node the vector
        for (ASTNode *parameter : parameterList) {
            delete parameter;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Prototype:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        if (!parameterList.empty()) {
            std::cout << std::string((depth + 1) * 2, ' ') << "Parameters:\n";
            // print each node the vector
            for (ASTNode *parameter : parameterList) {
                parameter->print((depth + 1) + 1);
            }
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Parameters\n";
        }
        // check if string is not empty
        if (!returnType.empty()) {
            std::cout << std::string((depth + 1) * 2, ' ') << "Return type: " << returnType << "\n";
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Return Type\n";
        }
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::vector<std::string> paramNames;
        std::vector<llvm::Type*> paramTypes;

        // generate the parameters
        for (ASTNode *parameter : parameterList) {
            std::unique_ptr<CodegenResult> parameterResult = parameter->codegen(ctx, builder, moduleLLVM);
            if (parameterResult == nullptr || parameterResult->resultType != PARAM_CODEGEN_RESULT) return nullptr;
            // check that the parameter is not redefined
            if (std::count(paramNames.begin(), paramNames.end(), parameterResult->param.identifier) > 0) {
                generator::fatal_error(std::chrono::high_resolution_clock::now(), "Cannot redefine parameter", "The parameter '" + parameterResult->param.identifier + "' is defined multiple times in the function '" + identifier + "'");
                return nullptr;
            }
            paramNames.push_back(parameterResult->param.identifier);
            paramTypes.push_back(parameterResult->param.type);
        }

        // check if the name is main and the return type is not i32 (or empty, because then it defaults to i32)
        if (identifier == "main" && !(returnType == "i32" || returnType.empty())) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid return type", "The main function can only return i32");
            return nullptr;
        }

        // get the llvm return type (if the identifier is main, then default to an i32)
        llvm::Type *llvmReturnType = identifier == "main" ? builder->getInt32Ty() : getLLVMType(returnType, builder);
        // return type, parameters, varargs
        llvm::FunctionType *fnType = llvm::FunctionType::get(llvmReturnType, paramTypes, false);
        llvm::Function *fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage, identifier, *moduleLLVM);

        // set the parameter names
        int i = 0;
        for (auto &arg : fn->args()) {
            arg.setName(paramNames[i++]);
        }

        return std::make_unique<CodegenResult>(fn, FUNCTION_CODEGEN_RESULT);
    }
};

class ASTFunctionDefinition : public ASTNode {
    ASTNode *prototype;
    ASTNode *body;

 public:
    ASTFunctionDefinition(ASTNode *prototype, ASTNode *body): prototype(prototype), body(body) {}
    ~ASTFunctionDefinition() {
        delete prototype;
        delete body;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Definition:\n";
        prototype->print(depth + 1);
        body->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // clear the scope stack when the function starts
        scopeStack.clear();

        // generate the function prototype and create the entry block
        std::unique_ptr<CodegenResult> prototypeResult = prototype->codegen(ctx, builder, moduleLLVM);
        if (prototypeResult == nullptr || prototypeResult->resultType != FUNCTION_CODEGEN_RESULT) return nullptr;
        llvm::Function *fn = prototypeResult->fn;
        auto entryBlock = llvm::BasicBlock::Create(*ctx, "entry", fn);
        builder->SetInsertPoint(entryBlock);

        for (auto &arg : fn->args()) {
            llvm::AllocaInst *allocaInstance = createEntryBlockAlloca(fn, std::string(arg.getName()), arg.getType());
            builder->CreateStore(&arg, allocaInstance);
            setAllocaInst(std::string(arg.getName()), allocaInstance);
        }

        body->codegen(ctx, builder, moduleLLVM);

        // check if the function is the main function
        bool isMainFunction = fn->getName() == llvm::StringRef("main");

        // if the function has a non-void return type (ignore all of this if it is the main function) then check if the function has a return statement
        llvm::Instruction *fnTerminator = builder->GetInsertBlock()->getTerminator();
        if (!isMainFunction && fn->getReturnType() != builder->getVoidTy() && (fnTerminator == nullptr || !isa<llvm::ReturnInst>(fnTerminator))) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(),
                "Missing return statement in function",
                "The function '" + std::string(fn->getName()) + "' has a non-void return type but does not have a return statement for each possible branch of execution");
            return nullptr;
        }

        // if the current block (last block to finish codegen in the function) does not have a terminator statement (check for any terminator and not just return statements)
        if (fnTerminator == nullptr) {
            // if the function name is main, then insert a return of 0, else just insert a return of void
            if (isMainFunction) {
                builder->CreateRet(builder->getInt32(0));
            } else {
                builder->CreateRetVoid();
            }
        }

        return nullptr;
    }
};

class ASTWhileLoop : public ASTNode {
    ASTNode *expression;
    ASTNode *loopBody;

 public:
    explicit ASTWhileLoop(ASTNode *expression, ASTNode *loopBody): expression(expression), loopBody(loopBody) {}
    ~ASTWhileLoop() {
        delete expression;
        delete loopBody;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "While Loop:\n";
        expression->print(depth + 1);
        loopBody->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // create the branches
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *conditionalBlock = llvm::BasicBlock::Create(*ctx, "loopcond", fn);
        llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(*ctx, "loopbody", fn);
        llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "loopcont", fn);

        // create the first branch
        builder->CreateBr(conditionalBlock);

        // emit the "loopcond" block
        builder->SetInsertPoint(conditionalBlock);
        // check if the expression has an invalid value
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in while loop", "The expression in the while loop has an invalid value");
            return nullptr;
        }
        // get the boolean value of the expression
        llvm::Value *condition = getBooleanValue(expressionResult->value, builder);
        // generate the condition
        builder->CreateCondBr(condition, loopBlock, mergeBlock);

        // update the loop context and emit the "loopbody" block
        builder->SetInsertPoint(loopBlock);
        loopContexts.emplace_back(nullptr, conditionalBlock, mergeBlock);  // save loop information for potential continue and break statements
        loopBody->codegen(ctx, builder, moduleLLVM);
        loopContexts.pop_back();  // restore the loop information for potential continue and break statements

        // update the loopBlock since the codegen of loopBody might change the current block
        loopBlock = builder->GetInsertBlock();
        // if there is no terminator instruction then generate the branch instruction
        if (loopBlock->getTerminator() == nullptr) builder->CreateBr(conditionalBlock);

        // emit the "merge" block
        builder->SetInsertPoint(mergeBlock);

        return nullptr;
    }
};

class ASTForLoop : public ASTNode {
    ASTNode *initStatement;
    ASTNode *expression;
    ASTNode *updateStatement;
    ASTNode *loopBody;

 public:
    explicit ASTForLoop(ASTNode *initStatement, ASTNode *expression, ASTNode *updateStatement, ASTNode *loopBody):  initStatement(initStatement), expression(expression), updateStatement(updateStatement), loopBody(loopBody) {}
    ~ASTForLoop() {
        delete initStatement;
        delete expression;
        delete updateStatement;
        delete loopBody;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "For Loop:\n";
        initStatement->print(depth + 1);
        expression->print(depth + 1);
        updateStatement->print(depth + 1);
        loopBody->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // create the branches
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *conditionalBlock = llvm::BasicBlock::Create(*ctx, "loopcond", fn);
        llvm::BasicBlock *loopBlock = llvm::BasicBlock::Create(*ctx, "loopbody", fn);
        llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "loopcont", fn);

        // start a new scope (to contain for example any variables defined in the init statement)
        scopeStack.emplace_back();

        // codegen the init statement (cannot contain return or anything alike)
        initStatement->codegen(ctx, builder, moduleLLVM);

        // create the first branch
        builder->CreateBr(conditionalBlock);

        // emit the "loopcond" block
        builder->SetInsertPoint(conditionalBlock);
        // check if the expression has an invalid value
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in for loop", "The expression in the for loop has an invalid value");
            return nullptr;
        }
        // get the boolean value of the expression
        llvm::Value *condition = getBooleanValue(expressionResult->value, builder);
        // generate the condition
        builder->CreateCondBr(condition, loopBlock, mergeBlock);

        // update the loop context and emit the "loopbody" block
        builder->SetInsertPoint(loopBlock);
        loopContexts.emplace_back(updateStatement, conditionalBlock, mergeBlock);  // save loop information for potential continue and break statements
        loopBody->codegen(ctx, builder, moduleLLVM);
        loopContexts.pop_back();  // restore the loop information for potential continue and break statements

        // update the loopBlock since the codegen of loopBody might change the current block
        loopBlock = builder->GetInsertBlock();
        // if there is no terminator instruction then generate the update statement and the branch instruction
        if (loopBlock->getTerminator() == nullptr) {
            // codegen the update statement (cannot contain return or anything alike)
            updateStatement->codegen(ctx, builder, moduleLLVM);
            builder->CreateBr(conditionalBlock);
        }

        // emit the "merge" block
        builder->SetInsertPoint(mergeBlock);

        // end the scope
        scopeStack.pop_back();

        return nullptr;
    }
};

class ASTReturnStatement : public ASTNode {
    ASTNode* expression;
    bool hasExpression;

 public:
    explicit ASTReturnStatement(ASTNode *expression): expression(expression), hasExpression(true) {}
    ASTReturnStatement(): hasExpression(false) {}
    ~ASTReturnStatement() {
        if (hasExpression) delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Return Statement:\n";
        if (hasExpression) {
            expression->print(depth + 1);
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Expression\n";
        }
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        llvm::Type *returnType;
        if (hasExpression) {
            std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
            if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
                generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in return statement", "The return statement does not have a valid expression");
                return nullptr;
            }
            builder->CreateRet(expressionResult->value);
            returnType = expressionResult->value->getType();
        } else {
            // if the return statement has no expression then generate a void return
            builder->CreateRetVoid();
            returnType = builder->getVoidTy();
        }

        // check if the types match
        if (returnType != builder->GetInsertBlock()->getParent()->getReturnType()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(),
                "Type mismatch in return statement",
               "The type of the return statement does not match the return type of the parent function '" + std::string(builder->GetInsertBlock()->getParent()->getName()) + "'");
            return nullptr;
        }

        return nullptr;
    }
};

class ASTContinueStatement : public ASTNode {
 public:
    ASTContinueStatement() = default;  // empty because there are no fields
    ~ASTContinueStatement() = default;  // empty because there are no fields

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Continue Statement:\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // if the continue statement is not inside a loop, then throw an error
        if (loopContexts.empty()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid continue statement", "The continue statement is not inside of a loop");
            return nullptr;
        }
        const LoopContext currentLoopContext = loopContexts.back();
        // only generate the update statement if the loop is a for loop
        if (currentLoopContext.isForLoop()) currentLoopContext.updateStatement->codegen(ctx, builder, moduleLLVM);
        builder->CreateBr(currentLoopContext.conditionalBlock);
        return nullptr;
    }
};

class ASTBreakStatement : public ASTNode {
 public:
    ASTBreakStatement() = default;  // empty because there are no fields
    ~ASTBreakStatement() = default;  // empty because there are no fields

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Break Statement:\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        // if the break statement is not inside a loop, then throw an error
        if (loopContexts.empty()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid break statement", "The break statement is not inside of a loop");
            return nullptr;
        }
        const LoopContext currentLoopContext = loopContexts.back();
        builder->CreateBr(currentLoopContext.mergeBlock);
        return nullptr;
    }
};

class ASTVariableDeclaration : public ASTNode {
    std::string identifier;
    std::string type;

 public:
    ASTVariableDeclaration(std::string identifier, std::string type): identifier(identifier), type(type) {}
    ~ASTVariableDeclaration() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Declaration:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Type: " << type << "\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        if (scopeStack.back()[identifier] != nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Variable is already declared", "The variable '" + identifier + "' is already declared");
            return nullptr;
        }
        // create an allocation for the variable. Do it in the entry block so that it can get optimized easily
        llvm::AllocaInst *allocaInstance = createEntryBlockAlloca(builder->GetInsertBlock()->getParent(), identifier, getLLVMType(type, builder));
        setAllocaInst(identifier, allocaInstance);
        return nullptr;
    }
};

class ASTVariableAssignment : public ASTNode {
    std::string identifier;
    ASTNode *expression;
    std::string operation;

 public:
    ASTVariableAssignment(std::string identifier, ASTNode *expression, std::string operation): identifier(identifier), expression(expression), operation(operation) {}
    ~ASTVariableAssignment() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Assignment:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        if (!operation.empty()) std::cout << std::string((depth + 1) * 2, ' ') << "Operation: " << operation << "\n";
        expression->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        llvm::AllocaInst *allocaInst = getAllocaInst(identifier);
        if (allocaInst == nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Variable is not declared", "Cannot assign a value to the variable '" + identifier + "' since it has not been declared");
            return nullptr;
        }

        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in variable assignment", "The expression in an assignment of variable '" + identifier + "' has an invalid value");
            return nullptr;
        }
        if (expressionResult->value->getType() != allocaInst->getAllocatedType()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Type mismatch in variable assignment", "Cannot assign a value to the variable '" + identifier + "' which has a different type");
            return nullptr;
        }
        llvm::Value *resultValue = operation.empty()
            ? expressionResult->value
            // create a binary operation with the loaded value (of the variable to assign) and the expression value
            : createBinaryOperation(builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst, identifier.c_str()), expressionResult->value, operation, builder);
        // store the value of the expression
        builder->CreateStore(resultValue, allocaInst);
        return std::make_unique<CodegenResult>(resultValue, VALUE_CODEGEN_RESULT);
    }
};

class ASTVariableDefinition : public ASTNode {
    std::string identifier;
    std::string type;
    ASTNode *expression;

 public:
    ASTVariableDefinition(std::string identifier, std::string type, ASTNode *expression): identifier(identifier), type(type), expression(expression) {}
    ~ASTVariableDefinition() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Definition:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Type: " << identifier << "\n";
        expression->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        if (scopeStack.back()[identifier] != nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Variable is already declared", "Cannot define the variable '" + identifier + "' since it is already declared");
            return nullptr;
        }
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in variable definition", "The expression in a variable definition for the variable '" + identifier + "' has an invalid value");
            return nullptr;
        }
        // find the resulting type of the definition
        llvm::Type *resultingType = type == "auto" ? expressionResult->value->getType() : getLLVMType(type, builder);
        // if the type is not auto, then cast from the expression type to the type
        llvm::Value *resultingValue = type == "auto" ? expressionResult->value : createCast(expressionResult->value, resultingType, builder);
        // allocate space for the variable and store the value of the expression
        llvm::AllocaInst *allocaInstance = createEntryBlockAlloca(builder->GetInsertBlock()->getParent(), identifier, resultingType);
        setAllocaInst(identifier, allocaInstance);
        builder->CreateStore(resultingValue, allocaInstance);
        return nullptr;
    }
};

class ASTIfStatement : public ASTNode {
    ASTNode *expression;
    ASTNode *body;

 public:
    explicit ASTIfStatement(ASTNode *expression, ASTNode *body): expression(expression), body(body) {}
    ~ASTIfStatement() {
        delete expression;
        delete body;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "If Statement:\n";
        expression->print(depth + 1);
        body->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in if statement", "The expression in the if statement has an invalid value");
            return nullptr;
        }

        // get the boolean value of the expression
        llvm::Value *condition = getBooleanValue(expressionResult->value, builder);

        // create the branches
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*ctx, "then", fn);
        llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);

        // create conditional branch
        builder->CreateCondBr(condition, thenBlock, mergeBlock);

        // emit the "then" block
        builder->SetInsertPoint(thenBlock);
        std::unique_ptr<CodegenResult> thenResult = body->codegen(ctx, builder, moduleLLVM);
        // update the thenBlock since the codegen of thenBody might change the current block
        thenBlock = builder->GetInsertBlock();
        // if there is no terminator instruction then generate the branch instruction
        if (thenBlock->getTerminator() == nullptr) builder->CreateBr(mergeBlock);

        // emit the "merge" block
        builder->SetInsertPoint(mergeBlock);

        return nullptr;
    }
};

class ASTIfElseStatement : public ASTNode {
    ASTNode *expression;
    ASTNode *thenBody;
    ASTNode *elseBody;

 public:
    explicit ASTIfElseStatement(ASTNode *expression, ASTNode *thenBody, ASTNode *elseBody): expression(expression), thenBody(thenBody), elseBody(elseBody) {}
    ~ASTIfElseStatement() {
        delete expression;
        delete thenBody;
        delete elseBody;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "If-else Statement:\n";
        expression->print(depth + 1);
        thenBody->print(depth + 1);
        elseBody->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid expression in if-else statement", "The expression in the if-else statement has an invalid value");
            return nullptr;
        }
        // get the boolean value of the expression
        llvm::Value *condition = getBooleanValue(expressionResult->value, builder);

        // create the branches
        llvm::Function *fn = builder->GetInsertBlock()->getParent();
        llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*ctx, "then", fn);
        llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*ctx, "else", fn);
        llvm::BasicBlock *mergeBlock = nullptr;

        // create conditional branch
        builder->CreateCondBr(condition, thenBlock, elseBlock);

        // emit the "then" block
        builder->SetInsertPoint(thenBlock);
        std::unique_ptr<CodegenResult> thenResult = thenBody->codegen(ctx, builder, moduleLLVM);
        // update the thenBlock since the codegen of thenBody might change the current block
        thenBlock = builder->GetInsertBlock();
        // if there is no terminator instruction then generate the mergeBlock and branch instruction
        if (thenBlock->getTerminator() == nullptr) {
            mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);
            builder->CreateBr(mergeBlock);
        }

        // emit the "else" block
        builder->SetInsertPoint(elseBlock);
        std::unique_ptr<CodegenResult> elseResult = elseBody->codegen(ctx, builder, moduleLLVM);
        // update the elseBlock since the codegen of elseBody might change the current block
        elseBlock = builder->GetInsertBlock();
        // if there is no terminator instruction then generate the mergeBlock and branch instruction
        if (elseBlock->getTerminator() == nullptr) {
            if (!mergeBlock) mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);
            builder->CreateBr(mergeBlock);
        }

        // emit the "merge" block if either the then or else blocks had a branch statement
        if (!mergeBlock) return nullptr;
        builder->SetInsertPoint(mergeBlock);

        return nullptr;
    }
};

class ASTFunctionCall : public ASTNode {
    std::string identifier;
    std::vector<ASTNode*> argumentList;

 public:
    ASTFunctionCall(std::string identifier, std::vector<ASTNode*> argumentList): identifier(identifier), argumentList(argumentList) {}
    ~ASTFunctionCall() {
        // delete each node the vector
        for (ASTNode *argument : argumentList) {
            delete argument;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Call:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        if (!argumentList.empty()) {
            std::cout << std::string((depth + 1) * 2, ' ') << "Arguments\n";
            // print each node the vector
            for (ASTNode *argument : argumentList) {
                argument->print(depth + 2);
            }
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Arguments\n";
        }
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        llvm::Function *calleeFn = moduleLLVM->getFunction(identifier);
        if (!calleeFn) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Unknown function referenced", "The function '" + identifier + "' could not be found");
            return nullptr;
        }

        // get the number of parameters (including if it has a vararg)
        unsigned numParams = calleeFn->getFunctionType()->getNumParams();
        // check if the arguments length equals the amount of parameters, or it is greater than the amount of parameters if the function has varargs
        bool argumentsLengthMatches = argumentList.size() == numParams || (calleeFn->isVarArg() && argumentList.size() >= numParams);
        // check if they don't match
        if (!argumentsLengthMatches) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Incorrect arguments passed", "The amount of arguments passed do not match the amount of parameters in the function '" + identifier + "'");
            return nullptr;
        }

        // get the number of fixed parameters (all parameters except varargs)
        unsigned numFixedParams = numParams - (calleeFn->isVarArg() ? 1 : 0);

        // generate the arguments
        std::vector<llvm::Value *> args;
        for (std::size_t i = 0; i < argumentList.size(); i++) {
            std::unique_ptr<CodegenResult> argumentResult = argumentList[i]->codegen(ctx, builder, moduleLLVM);
            // check if the value is valid
            if (argumentResult == nullptr || argumentResult->resultType != VALUE_CODEGEN_RESULT) {
                generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid function argument", "The " + std::to_string(i + 1) + "'th argument of the function '" + identifier + "' has an invalid value");
                return nullptr;
            }

            // check if the argument type match the parameter type
            if (i <= numFixedParams && argumentResult->value->getType() != calleeFn->getFunctionType()->getParamType(i)) {
                generator::fatal_error(std::chrono::high_resolution_clock::now(), "Incorrect arguments passed", "The argument types do not match the parameter types of the function '" + identifier + "'");
                return nullptr;
            }

            args.push_back(argumentResult->value);
        }
        // if the function has a void return type then just return nullptr else return the call result
        if (calleeFn->getFunctionType()->getReturnType() == builder->getVoidTy()) {
            builder->CreateCall(calleeFn, args);
            return nullptr;
        }
        return std::make_unique<CodegenResult>(builder->CreateCall(calleeFn, args, "calltmp"), VALUE_CODEGEN_RESULT);
    }
};
