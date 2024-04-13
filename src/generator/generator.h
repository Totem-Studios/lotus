// Copyright 2023 Pontus Henriksson & Neo Mannskär, 2024 Lucas Norman

#pragma once
#include <memory>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include "./ast.h"
#include "./parse.h"

class Generator {
 private:
    std::unique_ptr<llvm::IRBuilder<>> builder;
    std::unique_ptr<llvm::LLVMContext> ctx;
    std::unique_ptr<llvm::Module> moduleLLVM;

 public:
    explicit Generator(const std::string& filename) {
        ctx = std::make_unique<llvm::LLVMContext>();
        moduleLLVM = std::make_unique<llvm::Module>("LotusLLVM", *ctx);
        builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
        generate(filename);
    }

    void generate(const std::string& filename) {
        // create the ast
        std::unique_ptr<AST> &ast = parse(filename);
        std::cout << "[Parsed AST]\n";
        ast->print();
        // generates the LLVM IR for the ast
        ast->codegen(ctx, builder, moduleLLVM);

        std::cout << "\n[Generated IR]\n";
        moduleLLVM->print(llvm::outs(), nullptr);

        // save the module IR to a file
        // saveModuleToFile(TODO);
    }

 private:
    void saveModuleToFile(const std::string& filename) {
        std::error_code errorCode;
        llvm::raw_fd_ostream stream(filename, errorCode);
        moduleLLVM->print(stream, nullptr);
    }
};
