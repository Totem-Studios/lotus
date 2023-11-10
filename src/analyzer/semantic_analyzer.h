// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include "../parser/ast/node.h"

class SemanticAnalyzer {
 public:
    AST::Node root;
    SemanticAnalyzer(AST::Node _root, const std::chrono::high_resolution_clock::time_point& time): root(std::move(_root)) {
        std::cout << "SEMANTICS" << std::endl;
    }
    ~SemanticAnalyzer() = default;
};
