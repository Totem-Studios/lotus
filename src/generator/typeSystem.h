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

// helper function to get the resulting types from a binary operation
static Type getResultTypeFromBinaryOperation(
    const Type& leftType,
    const Type& rightType /* may be necessary for future operations */,
    const std::string& operation) {
    // of the operation is a boolean operation, then return a boolean (because
    // both sides of the operation are cast to boolean)
    if (operation == "&&" || operation == "||")
        return Type{"bool"};
    // otherwise just return any type, like the left one
    return leftType;
}

// helper function to create binary operations
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
        return builder->CreateAnd(
            typeSystem::getBooleanValue(leftValue, builder),
            typeSystem::getBooleanValue(rightValue, builder), "andtmp");
    } else if (operation == "||") {
        return builder->CreateOr(
            typeSystem::getBooleanValue(leftValue, builder),
            typeSystem::getBooleanValue(rightValue, builder), "ortmp");
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
}  // namespace typeSystem
