#pragma once

#include <vector>
#include <string>
#include "ast.h"

// create a wrapper struct around the std::vector, so that it can be used in the bison union
struct VectorWrapper {
    std::vector<ASTNode*> *vector;
    // construct empty vector
    VectorWrapper() {
        vector = new std::vector<ASTNode*>();
    }
    // construct vector with one node
    explicit VectorWrapper(ASTNode *firstNode) {
        vector = new std::vector<ASTNode*>();
        vector->push_back(firstNode);
    }
    ~VectorWrapper() {
        delete vector;
    }
    void push(ASTNode *node) {
        vector->push_back(node);
    }
    inline std::vector<ASTNode*> &getVector() { return *vector; }
};

// create a wrapper struct around the std::string, so that it can be used in the bison union
struct StringWrapper {
    std::string *string;
    // create a std::string from s and store it in string
    explicit StringWrapper(char *s) {
        string = new std::string(s);
    }
    ~StringWrapper() {
        delete string;
    }
    inline std::string &getString() { return *string; }
};
