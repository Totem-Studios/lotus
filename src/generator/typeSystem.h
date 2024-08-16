// Copyright 2024 Lucas Norman

#pragma once

#include <map>
#include <ranges>
#include <vector>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "../diagnostics/generator.h"

namespace typeSystem {

struct Type {
 private:
    std::string type;

 public:
    Type() = default;
    explicit Type(std::string type) : type(std::move(type)) {}
    Type(const Type& other) = default;

    bool operator!=(const Type& otherType) const {
        return type != otherType.type;
    }

    bool operator==(const Type& otherType) const {
        return type == otherType.type;
    }

    [[nodiscard]] std::string toString() const { return type; }
    [[nodiscard]] llvm::Type*
    toLLVMType(const std::unique_ptr<llvm::IRBuilder<>>& builder) const {
        // if the type is a pointer, then recursively get the pointer type until
        // it is not a pointer anymore
        if (isPointerType()) {
            // get the pointer type of the type when removing the asterisk (the
            // element type)
            return llvm::PointerType::get(getElementType().toLLVMType(builder),
                                          0);
        }

        if (type == "bool")
            return builder->getInt1Ty();
        if (type == "i8" || type == "char")
            return builder->getInt8Ty();
        if (type == "i16")
            return builder->getInt16Ty();
        if (type == "i32")
            return builder->getInt32Ty();
        if (type == "i64")
            return builder->getInt64Ty();
        if (type == "f32")
            return builder->getFloatTy();
        if (type == "f64")
            return builder->getDoubleTy();
        if (type == "str")
            return llvm::PointerType::get(builder->getInt8Ty(), 0);
        // return a void type
        return builder->getVoidTy();
    }
    [[nodiscard]] bool isSigned() const {
        return type != "u8" && type != "u16" && type != "u32" && type != "u64";
    }
    [[nodiscard]] bool isPointerType() const { return type.ends_with('*'); }
    [[nodiscard]] bool isVoidType() const { return type.empty(); }
    [[nodiscard]] bool isBooleanType() const { return type == "bool"; }
    // i8 to i64 and u8 to u64, also including char
    [[nodiscard]] bool isIntegerType() const {
        return type == "i8" ||
               type == "i16" ||
               type == "i32" ||
               type == "i64" ||
               type == "u8" ||
               type == "u16" ||
               type == "u32" ||
               type == "u64" ||
               type == "char";
    }
    [[nodiscard]] bool isFloatingPointType() const {
        return type == "f32" || type == "f64";
    }
    [[nodiscard]] Type createPointerType() const { return Type{type + "*"}; }
    [[nodiscard]] Type getElementType() const {
        return Type{type.substr(0, type.length() - 1)};
    }
};

// helper function to get the llvm::Value* from an integer (since it might vary
// in bit-width)
static Type getIntegerType(uint64_t number,
                           const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (number <= 2147483647) {
        // limit for signed i32
        return Type{"i32"};
    } else if (number <= 9223372036854775807) {
        // limit for signed i64
        return Type{"i64"};
    } else {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Invalid integer literal",
            "The integer is to large to be represented as an integer");
        return Type{""};
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
createCast(llvm::Value* value, const Type& sourceType, const Type& destinationType,
           const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    if (sourceType == destinationType) {
        // no cast needed
        return value;
    }

    llvm::Type* llvmDestinationType = destinationType.toLLVMType(builder);

    // if the source type is bool
    if (sourceType.isBooleanType()) {
        if (destinationType.isIntegerType()) {
            return builder->CreateCast(llvm::Instruction::ZExt, value, llvmDestinationType,
                                       "tmpcast");
        } else if (destinationType.isFloatingPointType()) {
            return builder->CreateCast(llvm::Instruction::UIToFP, value, llvmDestinationType,
                                       "tmpcast");
        }
    }

    // if the destination type is bool, then check if the value does not equal 0
    if (destinationType.isBooleanType()) {
        if (sourceType.isIntegerType()) {
            return builder->CreateICmpNE(
                value, llvm::ConstantInt::get(value->getType(), 0), "cmptozero");
        } else if (sourceType.isFloatingPointType()) {
            return builder->CreateFCmpONE(
                value, llvm::ConstantFP::get(value->getType(), 0), "cmptozero");
        }
    }

    // if the source type and destination type is an integer or floating point
    if ((sourceType.isIntegerType() || sourceType.isFloatingPointType()) &&
        (destinationType.isIntegerType() || destinationType.isFloatingPointType())) {
        llvm::CastInst::CastOps castOperation =
            llvm::CastInst::getCastOpcode(value, true, llvmDestinationType, true);
        return builder->CreateCast(castOperation, value, llvmDestinationType, "tmpcast");
    }

    // throw error, cast is not supported
    generator::fatal_error(
        std::chrono::high_resolution_clock::now(), "Invalid cast",
        "Cannot cast from '" + sourceType.toString() + "' to '" + destinationType.toString() + "'");
    return nullptr;
}

// helper function for getting the boolean representation of a llvm::Value*
static llvm::Value*
getBooleanValue(llvm::Value* value, const Type& sourceType,
                const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    return createCast(value, sourceType, Type{"bool"}, builder);
}

// helper function to create binary operations
static std::tuple<llvm::Value*, Type>
createBinaryOperation(llvm::Value* leftValue, llvm::Value* rightValue, const Type& leftType, const Type& rightType,
                      const std::string& operation,
                      const std::unique_ptr<llvm::IRBuilder<>>& builder) {
    // these can be performed with different types because both sides are cast
    // to booleans
    if (operation == "&&") {
        return {
            builder->CreateAnd(getBooleanValue(leftValue, leftType, builder), getBooleanValue(rightValue, rightType, builder), "andtmp"),
            Type{"bool"}
        };
    } else if (operation == "||") {
        return {
            builder->CreateOr(getBooleanValue(leftValue, leftType, builder), getBooleanValue(rightValue, rightType, builder), "ortmp"),
            Type{"bool"}
        };
    }

    // check if the left and right expression have the same type
    if (leftType != rightType) {
        generator::fatal_error(
            std::chrono::high_resolution_clock::now(),
            "Type mismatch in binary operation",
            "Cannot perform the binary operation '" + operation + "' with the types '" + leftType.toString() + "' and '" + rightType.toString() + "'");
        return {};
    }

    bool isFloatingPointOperation = leftType.isFloatingPointType();
    // if it is an integer that is not an i1 (boolean)
    bool isIntegerOperation = leftType.isIntegerType();

    llvm::Value* resultValue = nullptr;

    // these operations can only be performed if the types are the same
    if (operation == "+") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFAdd(leftValue, rightValue, "addfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateAdd(leftValue, rightValue, "addtmp");
        }
    } else if (operation == "-") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFSub(leftValue, rightValue, "subfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSub(leftValue, rightValue, "subtmp");
        }
    } else if (operation == "*") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFMul(leftValue, rightValue, "mulfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateMul(leftValue, rightValue, "multmp");
        }
    } else if (operation == "/") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFDiv(leftValue, rightValue, "divfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSDiv(leftValue, rightValue, "divtmp");
        }
    } else if (operation == "%") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFRem(leftValue, rightValue, "remfloattmp");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateSRem(leftValue, rightValue, "remtmp");
        }
    } else if (operation == "==") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOEQ(leftValue, rightValue,
                                          "cmpfloattmpequals");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpEQ(leftValue, rightValue, "cmptmpequals");
        }
    } else if (operation == "!=") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpONE(leftValue, rightValue,
                                          "cmpfloattmpnotequals");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpNE(leftValue, rightValue,
                                         "cmptmpnotequals");
        }
    } else if (operation == "<") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOLT(leftValue, rightValue,
                                          "cmpfloattmpless");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpSLT(leftValue, rightValue, "cmptmpless");
        }
    } else if (operation == ">") {
        if (isFloatingPointOperation) {
            resultValue = builder->CreateFCmpOGT(leftValue, rightValue,
                                          "cmpfloattmpgreater");
        } else if (isIntegerOperation) {
            resultValue = builder->CreateICmpSGT(leftValue, rightValue,
                                          "cmptmpgreater");
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
        generator::fatal_error(
                std::chrono::high_resolution_clock::now(), "Invalid binary operator",
                "The binary operator '" + operation +
                "' is not supported with the type '" + leftType.toString() + "'");
        return {};
    }
    // return the result value together with the resulting type (the leftType and rightType are the same here)
    return {resultValue, leftType};
}
}  // namespace typeSystem
