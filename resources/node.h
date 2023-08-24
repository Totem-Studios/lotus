//
// Created by neoma on 2023-08-24.
//

#pragma once
#include <string>
#include <vector>

enum node_type {
    NODE_ROOT,

};

class node {
public:
    node_type type;
    std::vector<node> children;
};

class parseStatement : public node {
public:
    parseStatement() {

    }
};