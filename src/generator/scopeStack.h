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

namespace scopes {
// to keep track of the variables that are available in the current
// scope/function when generating LLVM IR. Each map in the vector is a scope
static std::vector<std::map<std::string, llvm::AllocaInst*>> scopeStack;

// helper function to find a variable in the scopeStack
static llvm::AllocaInst* getAllocaInst(const std::string& identifier) {
    // go over each scope and check if the variable is defined (reverse the
    // scopeStack to check the most nested scopes first)
    for (auto& scope : std::ranges::reverse_view(scopeStack)) {
        auto it = scope.find(identifier);
        if (it != scope.end())
            return it->second;
    }
    return nullptr;
}

// helper function to set a variable in the scopeStack
static void setAllocaInst(const std::string& identifier,
                          llvm::AllocaInst* allocaInst) {
    if (scopeStack.empty())
        scopeStack.emplace_back();
    scopeStack.back()[identifier] = allocaInst;
}

inline void clearScopes() { scopeStack.clear(); }

inline void startScope() { scopeStack.emplace_back(); }

inline void endScope() {
    if (!scopeStack.empty()) {
        scopeStack.pop_back();
    }
}

inline bool existsInCurrentScope(const std::string& identifier) {
    if (scopeStack.empty())
        return false;
    return scopeStack.back().find(identifier) != scopeStack.back().end();
}
}  // namespace scopes
