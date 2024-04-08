// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <iostream>
#include <vector>

#include "../lexer/token.h"
#include "./ast/node.h"

#include "../../config/config.h"

class Parser {
    size_t index;
    std::vector<token> *tokens;

    [[nodiscard]] AST::Node parse(const std::chrono::high_resolution_clock::time_point &time) {
        AST::Node programRoot(NODE_ROOT);
        return programRoot;
    }

 public:
    AST::Node root;

    Parser(std::vector<token> *_tokens, const std::chrono::high_resolution_clock::time_point &time): index(0), tokens(_tokens), root(parse(time)) {
    }

    ~Parser() = default;
};
