// Copyright 2024 Lucas Norman

#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <ranges>
#include <vector>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "../parser/ast.h"
#include "loopContext.h"
#include "scopeStack.h"

#include "../diagnostics/generator.h"

// helper function to get the llvm::Type* from a type name/string
static llvm::Type*
getLLVMType(const std::string& type,
            const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    // if the type is a pointer, then recursively get the pointer type until it
    // is not a pointer anymore
    if (type.ends_with('*')) {
        std::string modifiedType = type.substr(0, type.length() - 1);
        // get the pointer type of the type when removing the asterisk
        return llvm::PointerType::get(getLLVMType(modifiedType, builder), 0);
    }
    if (type == "bool") {
        return builder->getInt1Ty();
    } else if (type == "i8" || type == "char") {
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
    } else if (type == "str") {
        return llvm::PointerType::get(builder->getInt8Ty(), 0);
    } else {
        // return a void type
        return builder->getVoidTy();
    }
}

// helper function to get the llvm::Value* from an integer (since it might vary
// in bit-width)
static llvm::Value*
getIntegerValue(uint64_t number,
                const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (number <= 2147483647) {
        // limit for signed i32
        return builder->getInt32(number);
    } else if (number <= 9223372036854775807) {
        // limit for signed i64
        return builder->getInt64(number);
    } else {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid integer literal",
            "The integer is to large to be represented as an integer");
        return nullptr;
    }
}

// helper function to create a llvm cast instruction
static llvm::Value*
createCast(llvm::Value* value, llvm::Type* type,
           const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    llvm::Type* srcType = value->getType();

    if (srcType == type) {
        // no cast needed
        return value;
    }

    // if the source type is bool
    if (srcType->isIntegerTy(1)) {
        if (type->isIntegerTy()) {
            return builder->CreateCast(llvm::Instruction::ZExt, value, type,
                                       "tmpcast");
        } else if (type->isFloatingPointTy()) {
            return builder->CreateCast(llvm::Instruction::UIToFP, value, type,
                                       "tmpcast");
        }
    }

    // if destination type is bool, then check if the value does not equal 0
    if (type->isIntegerTy(1)) {
        if (srcType->isIntegerTy()) {
            return builder->CreateICmpNE(
                value, llvm::ConstantInt::get(srcType, 0), "cmptozero");
        } else if (srcType->isFloatingPointTy()) {
            return builder->CreateFCmpONE(
                value, llvm::ConstantFP::get(srcType, 0), "cmptozero");
        }
    }

    // if the src type is int or float and type is int or float
    if ((srcType->isIntegerTy() || srcType->isFloatingPointTy()) &&
        (type->isIntegerTy() || type->isFloatingPointTy())) {
        llvm::CastInst::CastOps castOperation =
            llvm::CastInst::getCastOpcode(value, true, type, true);
        return builder->CreateCast(castOperation, value, type, "tmpcast");
    }

    // throw error, cast is not supported
    std::string stringSrcType;
    std::string stringType;
    llvm::raw_string_ostream stream1(stringSrcType);
    llvm::raw_string_ostream stream2(stringType);
    srcType->print(stream1);
    type->print(stream2);
    generator::fatal_error(
        std::chrono::high_resolution_clock::now(), "Invalid cast",
        "Cannot cast from '" + stringSrcType + "' to '" + stringType + "'");
    return nullptr;
}

// helper function for getting the boolean representation of a llvm::Value
static llvm::Value*
getBooleanValue(llvm::Value* value,
                const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    return createCast(value, builder->getInt1Ty(), builder);
}

// helper function to create an alloca instruction in the entry block of a
// function. Used with mutable variables
static llvm::AllocaInst* createEntryBlockAlloca(llvm::Function* fn,
                                                const std::string& variableName,
                                                llvm::Type* type) {
    llvm::IRBuilder<> temporaryBuilder(&fn->getEntryBlock(),
                                       fn->getEntryBlock().begin());
    return temporaryBuilder.CreateAlloca(type, nullptr, variableName);
}

// helper function to create multiplication
static llvm::Value*
createBinaryOperation(llvm::Value* leftValue, llvm::Value* rightValue,
                      const std::string& operation,
                      const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    bool isFloatingPointOperation = leftValue->getType()->isFloatingPointTy();
    // if it is an integer that is not an i1 (boolean)
    bool isIntegerOperation = leftValue->getType()->isIntegerTy() &&
                              !leftValue->getType()->isIntegerTy(1);

    // these can be performed with different types because both sides are cast
    // to booleans
    if (operation == "&&") {
        return builder->CreateAnd(getBooleanValue(leftValue, builder),
                                  getBooleanValue(rightValue, builder),
                                  "andtmp");
    } else if (operation == "||") {
        return builder->CreateOr(getBooleanValue(leftValue, builder),
                                 getBooleanValue(rightValue, builder), "ortmp");
    }

    // check if the left and right expression have the same type
    if (leftValue->getType() != rightValue->getType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Type mismatch in binary operation",
            "The left and right hand sides of the binary operator '" +
                operation + "' have different types");
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
            return builder->CreateFCmpOEQ(leftValue, rightValue,
                                          "cmpfloattmpequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpEQ(leftValue, rightValue, "cmptmpequals");
        }
    } else if (operation == "!=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpONE(leftValue, rightValue,
                                          "cmpfloattmpnotequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpNE(leftValue, rightValue,
                                         "cmptmpnotequals");
        }
    } else if (operation == "<") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOLT(leftValue, rightValue,
                                          "cmpfloattmpless");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSLT(leftValue, rightValue, "cmptmpless");
        }
    } else if (operation == ">") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOGT(leftValue, rightValue,
                                          "cmpfloattmpgreater");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSGT(leftValue, rightValue,
                                          "cmptmpgreater");
        }
    } else if (operation == "<=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOLE(leftValue, rightValue,
                                          "cmpfloattmplessequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSLE(leftValue, rightValue,
                                          "cmptmplessequals");
        }
    } else if (operation == ">=") {
        if (isFloatingPointOperation) {
            return builder->CreateFCmpOGE(leftValue, rightValue,
                                          "cmpfloattmpgreaterequals");
        } else if (isIntegerOperation) {
            return builder->CreateICmpSGE(leftValue, rightValue,
                                          "cmptmpgreaterequals");
        }
    }

    // if no operators matched, then throw an error
    std::string stringType;
    llvm::raw_string_ostream stream(stringType);
    leftValue->getType()->print(stream);
    generator::fatal_error(
        std::chrono::high_resolution_clock::now(), "Invalid binary operator",
        "The binary operator '" + operation +
            "' is not supported with the type '" + stringType + "'");
    return nullptr;
}

std::unique_ptr<CodegenResult> ASTVariableExpression::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    llvm::AllocaInst* allocaInst = scopes::getAllocaInst(identifier);
    if (allocaInst == nullptr) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(), "Unknown variable name",
            "The variable '" + identifier + "' could not be found");
        return nullptr;
    }
    return std::make_unique<CodegenResult>(
        builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst,
                            identifier.c_str()),
        R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTInteger::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                    const std::unique_ptr<llvm::IRBuilder<>>& builder,
                    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(getIntegerValue(number, builder),
                                           R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTBool::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                 const std::unique_ptr<llvm::IRBuilder<>>& builder,
                 const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(builder->getInt1(value),
                                           R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTFloat::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                  const std::unique_ptr<llvm::IRBuilder<>>& builder,
                  const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(
        llvm::ConstantFP::get(builder->getDoubleTy(), number),
        R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTString::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                   const std::unique_ptr<llvm::IRBuilder<>>& builder,
                   const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(
        builder->CreateGlobalStringPtr(text, "strlit"), R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTChar::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                 const std::unique_ptr<llvm::IRBuilder<>>& builder,
                 const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(builder->getInt8(character),
                                           R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTTypeCast::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                     const std::unique_ptr<llvm::IRBuilder<>>& builder,
                     const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    // check if the expression result is valid
    if (expressionResult == nullptr ||
        expressionResult->resultType != R_VALUE_CODEGEN_RESULT) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in type cast operator",
            "The expression in the type cast has an invalid value");
    }
    return std::make_unique<CodegenResult>(
        createCast(expressionResult->getValue(), getLLVMType(type, builder),
                   builder),
        R_VALUE_CODEGEN_RESULT);
}

// this could also be called a scope
std::unique_ptr<CodegenResult> ASTCompoundStatement::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // start a new scope
    scopes::startScope();

    // codegen each node in the vector
    for (ASTNode* statement : statementList) {
        // ignore the return value
        (void)statement->codegen(ctx, builder, moduleLLVM);
        // if the latest basic block has a terminal statement, then skip
        // generating the rest
        if (builder->GetInsertBlock()->getTerminator())
            break;
    }

    // end the scope
    scopes::endScope();
    return nullptr;
}

std::unique_ptr<CodegenResult> ASTBinaryOperator::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::unique_ptr<CodegenResult> leftResult =
        left->codegen(ctx, builder, moduleLLVM);
    std::unique_ptr<CodegenResult> rightResult =
        right->codegen(ctx, builder, moduleLLVM);
    // check if both the left and right results are valid
    if (leftResult == nullptr ||
        !leftResult->isValueCodegenResultType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid left hand side of binary operator",
                               "The left hand side of the binary operator '" +
                                   operation + "' has an invalid value");
        return nullptr;
    }
    if (rightResult == nullptr ||
        !rightResult->isValueCodegenResultType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid right hand side of binary operator",
                               "The right hand side of the binary operator '" +
                                   operation + "' has an invalid value");
        return nullptr;
    }
    return std::make_unique<CodegenResult>(
        createBinaryOperation(leftResult->getValue(), rightResult->getValue(), operation,
                              builder),
        R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTUnaryOperator::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    // check if the expression result is valid
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid expression in unary operator",
                               "The expression in the unary operator '" +
                                   operation + "' has an invalid value");
    }
    bool isFloatingPointOperation =
        expressionResult->getValue()->getType()->isFloatingPointTy();
    bool isIntegerOperation = expressionResult->getValue()->getType()->isIntegerTy();
    llvm::Value* resultValue = nullptr;
    if (operation == "!") {
        resultValue = builder->CreateNot(
            getBooleanValue(expressionResult->getValue(), builder), "nottmp");
    } else if (operation == "-") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFNeg(expressionResult->getValue(), "negtmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateNeg(expressionResult->getValue(), "negtmp");
        }
    } else if (operation == "+") {
        // does not change the value (but check if it is used with valid types
        // anyway)
        if (isFloatingPointOperation || isIntegerOperation)
            resultValue = expressionResult->getValue();
    }
    // check if the resultValue is a nullptr, then throw an error
    if (!resultValue) {
        std::string stringType;
        llvm::raw_string_ostream stream(stringType);
        expressionResult->getValue()->getType()->print(stream);
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(), "Invalid unary operator",
            "The unary operator '" + operation +
                "' is not supported with the type '" + stringType + "'");
        return nullptr;
    }
    return std::make_unique<CodegenResult>(resultValue, R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTIncrementDecrementOperator::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // get the alloca instance
    llvm::AllocaInst* allocaInstance = scopes::getAllocaInst(identifier);
    if (allocaInstance == nullptr) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(), "Unknown variable name",
            "The variable '" + identifier + "' could not be found");
        return nullptr;
    }

    // load the value
    llvm::Value* loadedValue = builder->CreateLoad(
        allocaInstance->getAllocatedType(), allocaInstance, identifier.c_str());

    llvm::Type* loadedValueType = loadedValue->getType();
    bool isIntegerType = loadedValueType->isIntegerTy();
    bool isFloatingPointType = loadedValueType->isFloatingPointTy();
    if (!isIntegerType && !isFloatingPointType) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid use of increment/decrement operator",
            "The variable '" + identifier +
                "' is not an integer or floating point value");
        return nullptr;
    }

    // to store the resulting value of the operation
    llvm::Value* resultValue;
    if (operation == "x++") {
        // set the resultValue to loaded value
        resultValue = loadedValue;
        // increment the value by one and store it
        builder->CreateStore(
            isIntegerType
                ? builder->CreateAdd(loadedValue,
                                     llvm::ConstantInt::get(loadedValueType, 1),
                                     "incrementtmp")
                : builder->CreateFAdd(loadedValue,
                                      llvm::ConstantFP::get(loadedValueType, 1),
                                      "incrementfloattmp"),
            allocaInstance);
    } else if (operation == "x--") {
        // set the resultValue to loaded value
        resultValue = loadedValue;
        // decrement the value by one and store it
        builder->CreateStore(
            isIntegerType
                ? builder->CreateSub(loadedValue,
                                     llvm::ConstantInt::get(loadedValueType, 1),
                                     "decrementtmp")
                : builder->CreateFSub(loadedValue,
                                      llvm::ConstantFP::get(loadedValueType, 1),
                                      "decrementfloattmp"),
            allocaInstance);
    } else if (operation == "++x") {
        // increment the value by one and set it to resultValue
        resultValue =
            isIntegerType
                ? builder->CreateAdd(loadedValue,
                                     llvm::ConstantInt::get(loadedValueType, 1),
                                     "incrementtmp")
                : builder->CreateFAdd(loadedValue,
                                      llvm::ConstantFP::get(loadedValueType, 1),
                                      "incrementfloattmp");
        // store the result value
        builder->CreateStore(resultValue, allocaInstance);
    } else if (operation == "--x") {
        // decrement the value by one and set it to resultValue
        resultValue =
            isIntegerType
                ? builder->CreateSub(loadedValue,
                                     llvm::ConstantInt::get(loadedValueType, 1),
                                     "decrementtmp")
                : builder->CreateFSub(loadedValue,
                                      llvm::ConstantFP::get(loadedValueType, 1),
                                      "decrementfloattmp");
        // store the result value
        builder->CreateStore(resultValue, allocaInstance);
    } else {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid increment/decrement operator",
                               "The operator '" + operation +
                                   "' is not supported");
        return nullptr;
    }
    return std::make_unique<CodegenResult>(resultValue, R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTAddressOfOperator::codegen(
        const std::unique_ptr<llvm::LLVMContext>& ctx,
        const std::unique_ptr<llvm::IRBuilder<>>& builder,
        const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // get the alloca instance
    llvm::AllocaInst* allocaInstance = scopes::getAllocaInst(identifier);
    if (allocaInstance == nullptr) {
        generator::fatal_error(
                std::chrono::high_resolution_clock::now(), "Unknown variable name",
                "Cannot take the address of the variable '" + identifier + "', because it could not be found");
        return nullptr;
    }
    return std::make_unique<CodegenResult>(allocaInstance, R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult>
ASTParameter::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                      const std::unique_ptr<llvm::IRBuilder<>>& builder,
                      const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    return std::make_unique<CodegenResult>(
        ParamCodegenResult(identifier, getLLVMType(type, builder)),
        PARAM_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTFunctionPrototype::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::vector<std::string> paramNames;
    std::vector<llvm::Type*> paramTypes;

    // generate the parameters
    for (ASTNode* parameter : parameterList) {
        std::unique_ptr<CodegenResult> parameterResult =
            parameter->codegen(ctx, builder, moduleLLVM);
        if (parameterResult == nullptr ||
            parameterResult->resultType != PARAM_CODEGEN_RESULT)
            return nullptr;
        // check that the parameter is not redefined
        if (std::count(paramNames.begin(), paramNames.end(),
                       parameterResult->param.identifier) > 0) {
            generator::fatal_error(
                std::chrono::high_resolution_clock::now(),
                "Cannot redefine parameter",
                "The parameter '" + parameterResult->param.identifier +
                    "' is defined multiple times in the function '" +
                    identifier + "'");
            return nullptr;
        }
        paramNames.push_back(parameterResult->param.identifier);
        paramTypes.push_back(parameterResult->param.type);
    }

    // check if the name is main and the return type is not i32 (or empty,
    // because then it defaults to i32)
    if (identifier == "main" && !(returnType == "i32" || returnType.empty())) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid return type",
                               "The main function can only return i32");
        return nullptr;
    }

    // get the llvm return type (if the identifier is main, then default to an
    // i32)
    llvm::Type* llvmReturnType = identifier == "main"
                                     ? builder->getInt32Ty()
                                     : getLLVMType(returnType, builder);
    // return type, parameters, varargs
    llvm::FunctionType* fnType =
        llvm::FunctionType::get(llvmReturnType, paramTypes, false);
    llvm::Function* fn = llvm::Function::Create(
        fnType, llvm::Function::ExternalLinkage, identifier, *moduleLLVM);

    // set the parameter names
    int i = 0;
    for (auto& arg : fn->args()) {
        arg.setName(paramNames[i++]);
    }

    return std::make_unique<CodegenResult>(fn, FUNCTION_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTFunctionDefinition::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // clear the scope stack when the function starts
    scopes::clearScopes();

    // generate the function prototype and create the entry block
    std::unique_ptr<CodegenResult> prototypeResult =
        prototype->codegen(ctx, builder, moduleLLVM);
    if (prototypeResult == nullptr ||
        prototypeResult->resultType != FUNCTION_CODEGEN_RESULT)
        return nullptr;
    llvm::Function* fn = prototypeResult->fn;
    auto entryBlock = llvm::BasicBlock::Create(*ctx, "entry", fn);
    builder->SetInsertPoint(entryBlock);

    for (auto& arg : fn->args()) {
        llvm::AllocaInst* allocaInstance = createEntryBlockAlloca(
            fn, std::string(arg.getName()), arg.getType());
        builder->CreateStore(&arg, allocaInstance);
        scopes::setAllocaInst(std::string(arg.getName()), allocaInstance);
    }

    // codegen the body, and ignore the return value
    (void)body->codegen(ctx, builder, moduleLLVM);

    // check if the function is the main function
    bool isMainFunction = fn->getName() == llvm::StringRef("main");

    // if the function has a non-void return type (ignore all of this if it is
    // the main function) then check if the function has a return statement
    llvm::Instruction* fnTerminator =
        builder->GetInsertBlock()->getTerminator();
    if (!isMainFunction && fn->getReturnType() != builder->getVoidTy() &&
        (fnTerminator == nullptr || !isa<llvm::ReturnInst>(fnTerminator))) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Missing return statement in function",
            "The function '" + std::string(fn->getName()) +
                "' has a non-void return type but does not have a return "
                "statement for each possible branch of execution");
        return nullptr;
    }

    // if the current block (last block to finish codegen in the function) does
    // not have a terminator statement (check for any terminator and not just
    // return statements)
    if (fnTerminator == nullptr) {
        // if the function name is main, then insert a return of 0, else just
        // insert a return of void
        if (isMainFunction) {
            builder->CreateRet(builder->getInt32(0));
        } else {
            builder->CreateRetVoid();
        }
    }

    return nullptr;
}

std::unique_ptr<CodegenResult>
ASTWhileLoop::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                      const std::unique_ptr<llvm::IRBuilder<>>& builder,
                      const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // create the branches
    llvm::Function* fn = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock* conditionalBlock =
        llvm::BasicBlock::Create(*ctx, "loopcond", fn);
    llvm::BasicBlock* loopBlock =
        llvm::BasicBlock::Create(*ctx, "loopbody", fn);
    llvm::BasicBlock* mergeBlock =
        llvm::BasicBlock::Create(*ctx, "loopcont", fn);

    // create the first branch
    builder->CreateBr(conditionalBlock);

    // emit the "loopcond" block
    builder->SetInsertPoint(conditionalBlock);
    // check if the expression has an invalid value
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in while loop",
            "The expression in the while loop has an invalid value");
        return nullptr;
    }
    // get the boolean value of the expression
    llvm::Value* condition = getBooleanValue(expressionResult->getValue(), builder);
    // generate the condition
    builder->CreateCondBr(condition, loopBlock, mergeBlock);

    // update the loop context and emit the "loopbody" block
    builder->SetInsertPoint(loopBlock);
    loopContext::startWhileLoop(conditionalBlock, mergeBlock);
    (void)loopBody->codegen(ctx, builder,
                            moduleLLVM);  // ignore the return value
    loopContext::endLoop();

    // update the loopBlock since the codegen of loopBody might change the
    // current block
    loopBlock = builder->GetInsertBlock();
    // if there is no terminator instruction then generate the branch
    // instruction
    if (loopBlock->getTerminator() == nullptr)
        builder->CreateBr(conditionalBlock);

    // emit the "merge" block
    builder->SetInsertPoint(mergeBlock);

    return nullptr;
}

std::unique_ptr<CodegenResult>
ASTForLoop::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                    const std::unique_ptr<llvm::IRBuilder<>>& builder,
                    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // create the branches
    llvm::Function* fn = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock* conditionalBlock =
        llvm::BasicBlock::Create(*ctx, "loopcond", fn);
    llvm::BasicBlock* loopBlock =
        llvm::BasicBlock::Create(*ctx, "loopbody", fn);
    llvm::BasicBlock* mergeBlock =
        llvm::BasicBlock::Create(*ctx, "loopcont", fn);

    // start a new scope
    // (to contain for example any variables defined in the init statement)
    scopes::startScope();

    // codegen the init statement, and ignore the return value (cannot contain
    // return or anything alike)
    (void)initStatement->codegen(ctx, builder, moduleLLVM);

    // create the first branch
    builder->CreateBr(conditionalBlock);

    // emit the "loopcond" block
    builder->SetInsertPoint(conditionalBlock);
    // check if the expression has an invalid value
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in for loop",
            "The expression in the for loop has an invalid value");
        return nullptr;
    }
    // get the boolean value of the expression
    llvm::Value* condition = getBooleanValue(expressionResult->getValue(), builder);
    // generate the condition
    builder->CreateCondBr(condition, loopBlock, mergeBlock);

    // update the loop context and emit the "loopbody" block
    builder->SetInsertPoint(loopBlock);
    loopContext::startForLoop(updateStatement, conditionalBlock, mergeBlock);
    // statements
    (void)loopBody->codegen(ctx, builder,
                            moduleLLVM);  // ignore the return value
    loopContext::endLoop();

    // update the loopBlock since the codegen of loopBody might change the
    // current block
    loopBlock = builder->GetInsertBlock();
    // if there is no terminator instruction then generate the update statement
    // and the branch instruction
    if (loopBlock->getTerminator() == nullptr) {
        // codegen the update statement, and ignore the return value (cannot
        // contain return or anything alike)
        (void)updateStatement->codegen(ctx, builder, moduleLLVM);
        builder->CreateBr(conditionalBlock);
    }

    // emit the "merge" block
    builder->SetInsertPoint(mergeBlock);

    // end the scope
    scopes::endScope();

    return nullptr;
}

std::unique_ptr<CodegenResult> ASTReturnStatement::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    llvm::Type* returnType;
    if (expression != nullptr) {
        std::unique_ptr<CodegenResult> expressionResult =
            expression->codegen(ctx, builder, moduleLLVM);
        if (expressionResult == nullptr ||
            !expressionResult->isValueCodegenResultType()) {
            generator::fatal_error(
                std::chrono::high_resolution_clock::now(),
                "Invalid expression in return statement",
                "The return statement does not have a valid expression");
            return nullptr;
        }
        builder->CreateRet(expressionResult->getValue());
        returnType = expressionResult->getValue()->getType();
    } else {
        // if the return statement has no expression then generate a void return
        builder->CreateRetVoid();
        returnType = builder->getVoidTy();
    }

    // check if the types match
    if (returnType != builder->GetInsertBlock()->getParent()->getReturnType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Type mismatch in return statement",
            "The type of the return statement does not match the return type "
            "of the parent function '" +
                std::string(builder->GetInsertBlock()->getParent()->getName()) +
                "'");
        return nullptr;
    }

    return nullptr;
}

std::unique_ptr<CodegenResult> ASTContinueStatement::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // if the continue statement is not inside a loop, then throw an error
    if (loopContext::currentlyNotInsideLoop()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid continue statement",
            "The continue statement is not inside of a loop");
        return nullptr;
    }
    const loopContext::LoopContext& currentLoopContext =
        loopContext::getCurrentLoopContext();
    // only generate the update statement if the loop is a for loop
    if (currentLoopContext.isForLoop()) {
        // ignore the return value
        (void)currentLoopContext.updateStatement->codegen(ctx, builder,
                                                          moduleLLVM);
    }
    builder->CreateBr(currentLoopContext.conditionalBlock);
    return nullptr;
}

std::unique_ptr<CodegenResult> ASTBreakStatement::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    // if the break statement is not inside a loop, then throw an error
    if (loopContext::currentlyNotInsideLoop()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid break statement",
                               "The break statement is not inside of a loop");
        return nullptr;
    }
    builder->CreateBr(loopContext::getCurrentLoopContext().mergeBlock);
    return nullptr;
}

std::unique_ptr<CodegenResult> ASTVariableDeclaration::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    if (scopes::existsInCurrentScope(identifier)) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Variable is already declared",
                               "The variable '" + identifier +
                                   "' is already declared");
        return nullptr;
    }
    // create an allocation for the variable. Do it in the entry block so that
    // it can get optimized easily
    llvm::AllocaInst* allocaInstance =
        createEntryBlockAlloca(builder->GetInsertBlock()->getParent(),
                               identifier, getLLVMType(type, builder));
    scopes::setAllocaInst(identifier, allocaInstance);
    return nullptr;
}

std::unique_ptr<CodegenResult> ASTVariableAssignment::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    llvm::AllocaInst* allocaInst = scopes::getAllocaInst(identifier);
    if (allocaInst == nullptr) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Variable is not declared",
                               "Cannot assign a value to the variable '" +
                                   identifier +
                                   "' since it has not been declared");
        return nullptr;
    }

    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid expression in variable assignment",
                               "The expression in an assignment of variable '" +
                                   identifier + "' has an invalid value");
        return nullptr;
    }
    if (expressionResult->getValue()->getType() != allocaInst->getAllocatedType()) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Type mismatch in variable assignment",
                               "Cannot assign a value to the variable '" +
                                   identifier + "' which has a different type");
        return nullptr;
    }
    llvm::Value* resultValue =
        operation.empty()
            ? expressionResult->getValue()
            // create a binary operation with the loaded value (of the variable
            // to assign) and the expression value
            : createBinaryOperation(
                  builder->CreateLoad(allocaInst->getAllocatedType(),
                                      allocaInst, identifier.c_str()),
                  expressionResult->getValue(), operation, builder);
    // store the value of the expression
    builder->CreateStore(resultValue, allocaInst);
    return std::make_unique<CodegenResult>(resultValue, R_VALUE_CODEGEN_RESULT);
}

std::unique_ptr<CodegenResult> ASTVariableDefinition::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    if (scopes::existsInCurrentScope(identifier)) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Variable is already declared",
                               "Cannot define the variable '" + identifier +
                                   "' since it is already declared");
        return nullptr;
    }
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in variable definition",
            "The expression in a variable definition for the variable '" +
                identifier + "' has an invalid value");
        return nullptr;
    }
    // find the resulting type of the definition
    llvm::Type* resultingType = type == "auto"
                                    ? expressionResult->getValue()->getType()
                                    : getLLVMType(type, builder);
    // if the type is not auto, then cast from the expression type to the type
    llvm::Value* resultingValue =
        type == "auto"
            ? expressionResult->getValue()
            : createCast(expressionResult->getValue(), resultingType, builder);
    // allocate space for the variable and store the value of the expression
    llvm::AllocaInst* allocaInstance = createEntryBlockAlloca(
        builder->GetInsertBlock()->getParent(), identifier, resultingType);
    scopes::setAllocaInst(identifier, allocaInstance);
    builder->CreateStore(resultingValue, allocaInstance);
    return nullptr;
}

std::unique_ptr<CodegenResult>
ASTIfStatement::codegen(const std::unique_ptr<llvm::LLVMContext>& ctx,
                        const std::unique_ptr<llvm::IRBuilder<>>& builder,
                        const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in if statement",
            "The expression in the if statement has an invalid value");
        return nullptr;
    }

    // get the boolean value of the expression
    llvm::Value* condition = getBooleanValue(expressionResult->getValue(), builder);

    // create the branches
    llvm::Function* fn = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*ctx, "then", fn);
    llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);

    // create conditional branch
    builder->CreateCondBr(condition, thenBlock, mergeBlock);

    // emit the "then" block
    builder->SetInsertPoint(thenBlock);
    std::unique_ptr<CodegenResult> thenResult =
        body->codegen(ctx, builder, moduleLLVM);
    // update the thenBlock since the codegen of thenBody might change the
    // current block
    thenBlock = builder->GetInsertBlock();
    // if there is no terminator instruction then generate the branch
    // instruction
    if (thenBlock->getTerminator() == nullptr)
        builder->CreateBr(mergeBlock);

    // emit the "merge" block
    builder->SetInsertPoint(mergeBlock);

    return nullptr;
}

std::unique_ptr<CodegenResult> ASTIfElseStatement::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    std::unique_ptr<CodegenResult> expressionResult =
        expression->codegen(ctx, builder, moduleLLVM);
    if (expressionResult == nullptr ||
        !expressionResult->isValueCodegenResultType()) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid expression in if-else statement",
            "The expression in the if-else statement has an invalid value");
        return nullptr;
    }
    // get the boolean value of the expression
    llvm::Value* condition = getBooleanValue(expressionResult->getValue(), builder);

    // create the branches
    llvm::Function* fn = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(*ctx, "then", fn);
    llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(*ctx, "else", fn);
    llvm::BasicBlock* mergeBlock = nullptr;

    // create conditional branch
    builder->CreateCondBr(condition, thenBlock, elseBlock);

    // emit the "then" block
    builder->SetInsertPoint(thenBlock);
    std::unique_ptr<CodegenResult> thenResult =
        thenBody->codegen(ctx, builder, moduleLLVM);
    // update the thenBlock since the codegen of thenBody might change the
    // current block
    thenBlock = builder->GetInsertBlock();
    // if there is no terminator instruction then generate the mergeBlock and
    // branch instruction
    if (thenBlock->getTerminator() == nullptr) {
        mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);
        builder->CreateBr(mergeBlock);
    }

    // emit the "else" block
    builder->SetInsertPoint(elseBlock);
    std::unique_ptr<CodegenResult> elseResult =
        elseBody->codegen(ctx, builder, moduleLLVM);
    // update the elseBlock since the codegen of elseBody might change the
    // current block
    elseBlock = builder->GetInsertBlock();
    // if there is no terminator instruction then generate the mergeBlock and
    // branch instruction
    if (elseBlock->getTerminator() == nullptr) {
        if (!mergeBlock)
            mergeBlock = llvm::BasicBlock::Create(*ctx, "ifcont", fn);
        builder->CreateBr(mergeBlock);
    }

    // emit the "merge" block if either the then or else blocks had a branch
    // statement
    if (!mergeBlock)
        return nullptr;
    builder->SetInsertPoint(mergeBlock);

    return nullptr;
}

std::unique_ptr<CodegenResult> ASTFunctionCall::codegen(
    const std::unique_ptr<llvm::LLVMContext>& ctx,
    const std::unique_ptr<llvm::IRBuilder<>>& builder,
    const std::unique_ptr<llvm::Module>& moduleLLVM) const {
    llvm::Function* calleeFn = moduleLLVM->getFunction(identifier);
    if (!calleeFn) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Unknown function referenced",
                               "The function '" + identifier +
                                   "' could not be found");
        return nullptr;
    }

    // get the number of parameters (including if it has a vararg)
    unsigned numParams = calleeFn->getFunctionType()->getNumParams();
    // check if the arguments length equals the amount of parameters, or it is
    // greater than the amount of parameters if the function has varargs
    bool argumentsLengthMatches =
        argumentList.size() == numParams ||
        (calleeFn->isVarArg() && argumentList.size() >= numParams);
    // check if they don't match
    if (!argumentsLengthMatches) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Incorrect arguments passed",
                               "The amount of arguments passed do not match "
                               "the amount of parameters in the function '" +
                                   identifier + "'");
        return nullptr;
    }

    // get the number of fixed parameters (all parameters except varargs)
    unsigned numFixedParams = numParams - (calleeFn->isVarArg() ? 1 : 0);

    // generate the arguments
    std::vector<llvm::Value*> args;
    for (std::size_t i = 0; i < argumentList.size(); i++) {
        std::unique_ptr<CodegenResult> argumentResult =
            argumentList[i]->codegen(ctx, builder, moduleLLVM);
        // check if the value is valid
        if (argumentResult == nullptr ||
            !argumentResult->isValueCodegenResultType()) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(),
                                   "Invalid function argument",
                                   "The " + std::to_string(i + 1) +
                                       "'th argument of the function '" +
                                       identifier + "' has an invalid value");
            return nullptr;
        }

        // check if the argument type match the parameter type
        if (i <= numFixedParams &&
            argumentResult->getValue()->getType() !=
                calleeFn->getFunctionType()->getParamType(i)) {
            generator::fatal_error(std::chrono::high_resolution_clock::now(),
                                   "Incorrect arguments passed",
                                   "The argument types do not match the "
                                   "parameter types of the function '" +
                                       identifier + "'");
            return nullptr;
        }

        args.push_back(argumentResult->getValue());
    }
    // if the function has a void return type then just return nullptr else
    // return the call result
    if (calleeFn->getFunctionType()->getReturnType() == builder->getVoidTy()) {
        builder->CreateCall(calleeFn, args);
        return nullptr;
    }
    return std::make_unique<CodegenResult>(
        builder->CreateCall(calleeFn, args, "calltmp"), R_VALUE_CODEGEN_RESULT);
}
