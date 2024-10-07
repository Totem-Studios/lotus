// Copyright 2024 Lucas Norman

#pragma once

#include <map>
#include <ranges>
#include <regex>
#include <vector>


#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "scopeStack.h"
#include "typeSystem.h"


#include "../diagnostics/generator.h"

namespace casting {

// helper function to create a llvm cast instruction
static llvm::Value*
createCast(llvm::Value* value, const typeSystem::Type& sourceType,
           const typeSystem::Type& destinationType,
           const std::unique_ptr<llvm::IRBuilder<>>& builder,
           // an optional pointer to the value (used when casting arrays)
           llvm::Value* pointer = nullptr) {
    if (sourceType == destinationType) {
        // no cast needed
        return value;
    }

    llvm::Type* llvmDestinationType = destinationType.toLLVMType(builder);

    // if the source type is bool
    if (sourceType.isBooleanType()) {
        if (destinationType.isIntegerType()) {
            return builder->CreateCast(llvm::Instruction::ZExt, value,
                                       llvmDestinationType, "tmpcast");
        } else if (destinationType.isFloatingPointType()) {
            return builder->CreateCast(llvm::Instruction::UIToFP, value,
                                       llvmDestinationType, "tmpcast");
        }
    }

    // if the destination type is bool, then check if the value does not equal 0
    if (destinationType.isBooleanType()) {
        if (sourceType.isIntegerType()) {
            return builder->CreateICmpNE(
                value, llvm::ConstantInt::get(value->getType(), 0),
                "cmptozero");
        } else if (sourceType.isFloatingPointType()) {
            return builder->CreateFCmpONE(
                value, llvm::ConstantFP::get(value->getType(), 0), "cmptozero");
        }
    }

    // if the source type and destination type is an integer or floating point
    if ((sourceType.isIntegerType() || sourceType.isFloatingPointType()) &&
        (destinationType.isIntegerType() ||
         destinationType.isFloatingPointType())) {
        llvm::CastInst::CastOps castOperation = llvm::CastInst::getCastOpcode(
            value, true, llvmDestinationType, true);
        return builder->CreateCast(castOperation, value, llvmDestinationType,
                                   "tmpcast");
    }

    // throw error, cast is not supported
    generator::fatal_error(std::chrono::high_resolution_clock::now(),
                           "Invalid cast",
                           "Cannot cast from '" + sourceType.toString() +
                               "' to '" + destinationType.toString() + "'");
    return nullptr;
}

// helper function for getting the boolean representation of a llvm::Value*
static llvm::Value*
toBoolean(llvm::Value* value, const typeSystem::Type& sourceType,
          const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    return createCast(value, sourceType, typeSystem::Type{"bool"}, builder);
}

// helper function to create binary operations
static std::tuple<llvm::Value*, typeSystem::Type>
createBinaryOperation(llvm::Value* leftValue, llvm::Value* rightValue,
                      const typeSystem::Type& leftType,
                      const typeSystem::Type& rightType,
                      const std::string& operation,
                      const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    // these can be performed with different types because both sides are cast
    // to booleans
    if (operation == "&&") {
        return {builder->CreateAnd(toBoolean(leftValue, leftType, builder),
                                   toBoolean(rightValue, rightType, builder),
                                   "andtmp"),
                typeSystem::Type{"bool"}};
    } else if (operation == "||") {
        return {builder->CreateOr(toBoolean(leftValue, leftType, builder),
                                  toBoolean(rightValue, rightType, builder),
                                  "ortmp"),
                typeSystem::Type{"bool"}};
    }

    // check if the left and right expression have the same type
    if (leftType != rightType) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Type mismatch in binary operation",
                               "Cannot perform the binary operation '" +
                                   operation + "' with the types '" +
                                   leftType.toString() + "' and '" +
                                   rightType.toString() + "'");
        return {};
    }

    bool isFloatingPointOperation = leftType.isFloatingPointType();
    // if it is an integer that is not an i1 (boolean)
    bool isIntegerOperation = leftType.isIntegerType();

    llvm::Value* resultValue = nullptr;

    // these operations can only be performed if the types are the same
    if (operation == "+") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFAdd(leftValue, rightValue, "addfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (operation == "-") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFSub(leftValue, rightValue, "subfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (operation == "*") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFMul(leftValue, rightValue, "mulfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (operation == "/") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFDiv(leftValue, rightValue, "divfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSDiv(leftValue, rightValue, "divtmp");
        }
    } else if (operation == "%") {
        if (isFloatingPointOperation) {
            resultValue =
                builder->CreateFRem(leftValue, rightValue, "remfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSRem(leftValue, rightValue, "remtmp");
        }
    } else if (operation == "==") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOEQ(leftValue, rightValue,
                                                 "cmpfloattmpequals");
        } else if (isIntegerOperation) {
            resultValue =
                builder->CreateICmpEQ(leftValue, rightValue, "cmptmpequals");
        }
    } else if (operation == "!=") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpONE(leftValue, rightValue,
                                                 "cmpfloattmpnotequals");
        } else if (isIntegerOperation) {
            resultValue =
                builder->CreateICmpNE(leftValue, rightValue, "cmptmpnotequals");
        }
    } else if (operation == "<") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOLT(leftValue, rightValue,
                                                 "cmpfloattmpless");
        } else if (isIntegerOperation) {
            resultValue =
                builder->CreateICmpSLT(leftValue, rightValue, "cmptmpless");
        }
    } else if (operation == ">") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOGT(leftValue, rightValue,
                                                 "cmpfloattmpgreater");
        } else if (isIntegerOperation) {
            resultValue =
                builder->CreateICmpSGT(leftValue, rightValue, "cmptmpgreater");
        }
    } else if (operation == "<=") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOLE(leftValue, rightValue,
                                                 "cmpfloattmplessequals");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpSLE(leftValue, rightValue,
                                                 "cmptmplessequals");
        }
    } else if (operation == ">=") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOGE(leftValue, rightValue,
                                                 "cmpfloattmpgreaterequals");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpSGE(leftValue, rightValue,
                                                 "cmptmpgreaterequals");
        }
    }

    // if no operators matched, then throw an error
    if (resultValue == nullptr) {
        generator::fatal_error(std::chrono::high_resolution_clock::now(),
                               "Invalid binary operator",
                               "The binary operator '" + operation +
                                   "' is not supported with the type '" +
                                   leftType.toString() + "'");
        return {};
    }
    // return the result value together with the resulting type (the leftType
    // and rightType are the same here)
    return {resultValue, leftType};
}
}  // namespace casting
