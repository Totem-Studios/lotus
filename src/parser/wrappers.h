// Copyright 2024 Lucas Norman

#pragma once

#include <regex>
#include <string>
#include <vector>

#include "ast.h"

// create a wrapper struct around the std::vector,
// so that it can be used in the bison union
struct VectorWrapper {
    std::vector<ASTNode*>* vector;

    // construct empty vector
    VectorWrapper() { vector = new std::vector<ASTNode*>(); }

    // construct vector with one node
    explicit VectorWrapper(ASTNode* firstNode) {
        vector = new std::vector<ASTNode*>();
        vector->push_back(firstNode);
    }

    ~VectorWrapper() { delete vector; }

    void push(ASTNode* node) const { vector->push_back(node); }

    [[nodiscard]] inline std::vector<ASTNode*>& getVector() const {
        return *vector;
    }
};

static std::string* replaceEscape(const std::string& string) {
    // replace all "\\n" with "\n"
    std::string modifiedString =
        std::regex_replace(string, std::regex("\\\\n"), "\n");
    // replace all "\\t" with "\t"
    modifiedString =
        std::regex_replace(modifiedString, std::regex("\\\\t"), "\t");
    return new std::string(std::move(modifiedString));
}

// create a wrapper struct around the std::string,
// so that it can be used in the bison union
struct StringWrapper {
    std::string* string;

    // create a std::string from s and store it in string
    explicit StringWrapper(char* s) {
        std::string str(s);
        // to replace \\n, \\t and so on with just one \ so it works correctly
        string = replaceEscape(str);
    }

    // create a std::string from s and length and store it in string
    explicit StringWrapper(char* s, int length) {
        std::string str(s, length);
        // to replace \\n, \\t and so on with just one \ so it works correctly
        string = replaceEscape(str);
    }

    ~StringWrapper() { delete string; }

    [[nodiscard]] inline std::string& getString() const { return *string; }
};
