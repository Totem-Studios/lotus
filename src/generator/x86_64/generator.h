// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <vector>
#include <fstream>

#include "../../parser/ast/node.h"

class Generator {
 public:
    AST::Node root;
    std::string x86_64;

    [[maybe_unused]] void createAsmFile(const std::string& outputFile) {
        std::ofstream outputStream(outputFile + ".s");
        outputStream << x86_64;
        outputStream.close();
    }

    Generator(AST::Node _root, const std::chrono::high_resolution_clock::time_point& time): root(std::move(_root)) {
        showTree(&root);
    }
};
