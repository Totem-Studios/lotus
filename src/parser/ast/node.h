// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <string>
#include <utility>
#include <vector>
#include <chrono>
#include <unordered_map>

#include "../../lexer/token.h"

#include "../../diagnostics/parser.h"

enum nodeType {
    NODE_ROOT = -1,
    NODE_NONE,
    NODE_ID,
    NODE_FN_DECLARATION,
    NODE_FNN_DECLARATION,
    NODE_STRUCT_DECLARATION,
    NODE_SUBJECT,
    NODE_FN_ARGS,

    NODE_STATEMENT,
};

namespace AST {
class Node {
 public:
        nodeType type;
        std::vector<Node*> children;
        explicit Node(nodeType _type): type(_type) {}
        virtual ~Node() = default;
};

    static inline std::string matchNodeEnum(const nodeType& n) {
        static std::unordered_map<nodeType, std::string> nodes {
                {NODE_ROOT, "NODE_ROOT"},
                {NODE_ID, "NODE_ID"},
                {NODE_FN_DECLARATION, "NODE_FN_DECLARATION"},
                {NODE_FNN_DECLARATION, "NODE_FNN_DECLARATION"},
        };
        auto typeIterator = nodes.find(n);
        if (typeIterator != nodes.end()) {
            return typeIterator->second;
        }
        return "NODE_NONE";
    }

    inline void showTree(const Node* node) {
        static unsigned short junctionDepth = 0;
        std::cout << std::string(junctionDepth, '\t') << (junctionDepth == 0 ? "" : "\\_ ") << matchNodeEnum(node->type) << std::endl;
        for (const AST::Node *n : node->children) {
            junctionDepth++;
            showTree(n);
        }
        junctionDepth--;
    }

}  // namespace
