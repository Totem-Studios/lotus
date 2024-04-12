#pragma once

#include <iostream>
#include <memory>
#include <vector>
#include <map>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include "../diagnostics/generator.h"

// to keep track of the variables that are available in the current scope/function when generating LLVM IR
static std::map<std::string, llvm::Value*> namedValues;

// helper function to get the llvm::Type* from a type name/string
static llvm::Type *getLLVMType(const std::string& type, const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (type == "i32") {
        return builder->getInt32Ty();
    } else if (type == "f32") {
        return builder->getFloatTy();
    } else {
        // return a void type
        return builder->getVoidTy();
    }
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

enum ASTNodeType {
    AST_VAR_EXPRESSION,
    AST_NUMBER,
    AST_BINARY_OPERATOR,
    AST_COMPOUND_STATEMENT,
    AST_PARAM,
    AST_FN_PROTOTYPE,
    AST_FN_DEFINITION,
    AST_RETURN_STATEMENT,
    AST_FN_CALL
};

class ASTNode {
 public:
    const ASTNodeType nodeType;

    explicit ASTNode(ASTNodeType nodeType): nodeType(nodeType) {}
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

class ASTVariableExpression : public ASTNode {
    std::string identifier;

 public:
    explicit ASTVariableExpression(std::string identifier): ASTNode(AST_VAR_EXPRESSION), identifier(identifier) {}
    ~ASTVariableExpression() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Expression: " << identifier << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        llvm::Value *value = namedValues[identifier];
        if (value == nullptr) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Unknown variable name", "The variable '" + identifier + "' could not be found");
            return nullptr;
        }
        return std::make_unique<CodegenResult>(value, VALUE_CODEGEN_RESULT);
    }
};

class ASTNumber : public ASTNode {
    int number;

 public:
    explicit ASTNumber(int number): ASTNode(AST_NUMBER), number(number) {}
    ~ASTNumber() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Number: " << number << '\n';
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        return std::make_unique<CodegenResult>(builder->getInt32(number), VALUE_CODEGEN_RESULT);
    }
};

class ASTCompoundStatement : public ASTNode {
    std::vector<ASTNode*> statementList;

 public:
    explicit ASTCompoundStatement(std::vector<ASTNode*> statementList): ASTNode(AST_COMPOUND_STATEMENT), statementList(statementList) {}
    // also add an empty constructor for no statements
    ASTCompoundStatement(): ASTNode(AST_COMPOUND_STATEMENT) {}
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
        // codegen node in the vector
        for (ASTNode *statement : statementList) {
            statement->codegen(ctx, builder, moduleLLVM);
        }
        return nullptr;
    }
};

class ASTBinaryOperator : public ASTNode {
    ASTNode *left;
    ASTNode *right;
    char operation;

 public:
    ASTBinaryOperator(ASTNode *left, ASTNode *right, char operation): ASTNode(AST_BINARY_OPERATOR), left(left), right(right), operation(operation) {}
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
        if (leftResult == nullptr || leftResult->resultType != VALUE_CODEGEN_RESULT || rightResult == nullptr || rightResult->resultType != VALUE_CODEGEN_RESULT) return nullptr;
        llvm::Value *resultValue;
        switch (operation) {
            case '+':
                resultValue = builder->CreateAdd(leftResult->value, rightResult->value, "addtmp");
                break;
            case '-':
                resultValue = builder->CreateSub(leftResult->value, rightResult->value, "subtmp");
                break;
            case '*':
                resultValue = builder->CreateMul(leftResult->value, rightResult->value, "multmp");
                break;
            default:
                generator::fatal_error(std::chrono::high_resolution_clock::now(), "Invalid binary operator", "The operator '" + std::string(1, operation) + "' is not supported");
                return nullptr;
                break;
        }
        return std::make_unique<CodegenResult>(resultValue, VALUE_CODEGEN_RESULT);
    }
};

class ASTParameter : public ASTNode {
    std::string identifier;
    std::string type;

 public:
    ASTParameter(std::string identifier, std::string type): ASTNode(AST_PARAM), identifier(identifier), type(type) {}
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
    ASTFunctionPrototype(std::string identifier, std::vector<ASTNode*> parameterList, std::string returnType): ASTNode(AST_FN_PROTOTYPE), identifier(identifier), parameterList(parameterList), returnType(returnType) {}
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
        if (!returnType.empty())
            std::cout << std::string((depth + 1) * 2, ' ') << "Return type: " << returnType << "\n";
        else
            std::cout << std::string((depth + 1) * 2, ' ') << "No Return Type\n";
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::vector<std::string> paramNames;
        std::vector<llvm::Type*> paramTypes;

        // generate the parameters
        for (ASTNode *parameter : parameterList) {
            std::unique_ptr<CodegenResult> parameterResult = parameter->codegen(ctx, builder, moduleLLVM);
            if (parameterResult == nullptr || parameterResult->resultType != PARAM_CODEGEN_RESULT) return nullptr;
            paramNames.push_back(parameterResult->param.identifier);
            paramTypes.push_back(parameterResult->param.type);
        }

        // return type, parameters, varargs
        llvm::FunctionType *fnType = llvm::FunctionType::get(getLLVMType(returnType, builder), paramTypes, false);
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
    ASTFunctionDefinition(ASTNode *prototype, ASTNode *body): ASTNode(AST_FN_DEFINITION), prototype(prototype), body(body) {}
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
        // generate the function prototype and create the entry block
        std::unique_ptr<CodegenResult> prototypeResult = prototype->codegen(ctx, builder, moduleLLVM);
        if (prototypeResult == nullptr || prototypeResult->resultType != FUNCTION_CODEGEN_RESULT) return nullptr;
        llvm::Function *fn = prototypeResult->fn;
        auto entry = llvm::BasicBlock::Create(*ctx, "entry", fn);
        builder->SetInsertPoint(entry);

        // set the named values for the parameters (before the body is generated)
        namedValues.clear();
        for (auto &arg : fn->args()) {
            namedValues[std::string(arg.getName())] = &arg;
        }

        body->codegen(ctx, builder, moduleLLVM);

        return nullptr;
    }
};

class ASTReturnStatement : public ASTNode {
    ASTNode *expression;

 public:
    explicit ASTReturnStatement(ASTNode *expression): ASTNode(AST_RETURN_STATEMENT), expression(expression) {}
    ~ASTReturnStatement() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Return Statement:\n";
        expression->print(depth + 1);
    }

    std::unique_ptr<CodegenResult> codegen(const std::unique_ptr<llvm::LLVMContext>& ctx, const std::unique_ptr<llvm::IRBuilder<>>& builder, const std::unique_ptr<llvm::Module>& moduleLLVM) const override {
        std::unique_ptr<CodegenResult> expressionResult = expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr || expressionResult->resultType != VALUE_CODEGEN_RESULT) return nullptr;
        builder->CreateRet(expressionResult->value);
        return nullptr;
    }
};

class ASTFunctionCall : public ASTNode {
    std::string identifier;
    std::vector<ASTNode*> argumentList;

 public:
    ASTFunctionCall(std::string identifier, std::vector<ASTNode*> argumentList): ASTNode(AST_FN_CALL), identifier(identifier), argumentList(argumentList) {}
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

        // If argument mismatch error.
        if (calleeFn->arg_size() != argumentList.size()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(), "Incorrect arguments passed", "The arguments passed do not match the parameters of function '" + identifier + "'");
            return nullptr;
        }

        std::vector<llvm::Value *> args;

        // generate the arguments
        for (ASTNode *argument : argumentList) {
            std::unique_ptr<CodegenResult> argumentResult = argument->codegen(ctx, builder, moduleLLVM);
            if (argumentResult == nullptr || argumentResult->resultType != VALUE_CODEGEN_RESULT) return nullptr;
            args.push_back(argumentResult->value);
        }

        return std::make_unique<CodegenResult>(builder->CreateCall(calleeFn, args, "calltmp"), VALUE_CODEGEN_RESULT);
    }
};
