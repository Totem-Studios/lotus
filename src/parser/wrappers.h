// Copyright 2024 Lucas Norman

#pragma once

#include <regex>
#include <string>
#include <vector>

#include "ast.h"

// create a wrapper struct around the std::vector,
// so that it can be used in the bison union
struct VectorWrapper {
 private:
    std::vector<ASTNode*> vector;

 public:
    // construct empty vector
    VectorWrapper() = default;

    // construct vector with one node
    explicit VectorWrapper(ASTNode* firstNode) : vector({firstNode}) {}

    void push(ASTNode* node) { vector.push_back(node); }

    [[nodiscard]] const std::vector<ASTNode*>& getVector() const {
        return vector;
    }
};

// create a wrapper struct around the std::string,
// so that it can be used in the bison union
struct StringWrapper {
 private:
    std::string string;

    void replaceEscape() {
        // replace all "\\n" with "\n"
        string = std::regex_replace(string, std::regex("\\\\n"), "\n");
        // replace all "\\t" with "\t"
        string = std::regex_replace(string, std::regex("\\\\t"), "\t");
    }

 public:
    // create a std::string from s and store it in string
    explicit StringWrapper(const char* s) : string(s) { replaceEscape(); }

    // create a std::string from s and length and store it in string
    explicit StringWrapper(const char* s, size_t length) : string(s, length) {
        replaceEscape();
    }

    [[nodiscard]] const std::string& getString() const { return string; }
};
