#pragma once

#include <iostream>
#include <memory>
#include <vector>

enum ASTNodeType {
    AST_VAR_EXPRESSION,
    AST_NUMBER,
    AST_BINARY_OPERATOR,
    AST_COMPOUND_STATEMENT,
    AST_PARAM,
    AST_FN_PROTOTYPE,
    AST_FN_DEFINITION,
    AST_RETURN_STATEMENT,
    AST_FN_CALL
};

class ASTNode {
public:
    const ASTNodeType nodeType;

    explicit ASTNode(ASTNodeType nodeType): nodeType(nodeType) {}
    virtual ~ASTNode() {}

    virtual void print(int depth) const = 0;
};

class AbstractSyntaxTree {
    std::vector<ASTNode*> rootNodes;

public:
    explicit AbstractSyntaxTree(std::vector<ASTNode*> rootNodes): rootNodes(rootNodes) {}
    ~AbstractSyntaxTree() {
        // delete each node the vector
        for (ASTNode *node : rootNodes) {
            delete node;
        }
    }

    void print() const {
        // print each node the vector
        for (ASTNode *node : rootNodes) {
            node->print(0);
        }
    }
};

class ASTVariableExpression : public ASTNode {
    std::string identifier;

public:
    explicit ASTVariableExpression(std::string identifier): ASTNode(AST_VAR_EXPRESSION), identifier(identifier) {}
    ~ASTVariableExpression() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Variable Expression: " << identifier << '\n';
    }
};

class ASTNumber : public ASTNode {
    int number;

public:
    explicit ASTNumber(int number): ASTNode(AST_NUMBER), number(number) {}
    ~ASTNumber() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Number: " << number << '\n';
    }
};

class ASTCompoundStatement : public ASTNode {
    std::vector<ASTNode*> statementList;

public:
    explicit ASTCompoundStatement(std::vector<ASTNode*> statementList): ASTNode(AST_COMPOUND_STATEMENT), statementList(statementList) {}
    // also add an empty constructor for no statements
    ASTCompoundStatement(): ASTNode(AST_COMPOUND_STATEMENT) {}
    ~ASTCompoundStatement() {
        // delete each node the vector
        for (ASTNode *statement : statementList) {
            delete statement;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Compound Statement:\n";
        // print each node the vector
        for (ASTNode *statement : statementList) {
            statement->print(depth + 1);
        }
    }
};

class ASTBinaryOperator : public ASTNode {
    ASTNode *left;
    ASTNode *right;
    char operation;

public:
    ASTBinaryOperator(ASTNode *left, ASTNode *right, char operation): ASTNode(AST_BINARY_OPERATOR), left(left), right(right), operation(operation) {}
    ~ASTBinaryOperator() {
        delete left;
        delete right;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Binary Operator: " << operation << '\n';
        left->print(depth + 1);
        right->print(depth + 1);
    }
};

class ASTParameter : public ASTNode {
    std::string identifier;
    std::string type;

public:
    ASTParameter(std::string identifier, std::string type): ASTNode(AST_PARAM), identifier(identifier), type(type) {}
    ~ASTParameter() {}  // no child nodes

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Parameter:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Type: " << type << "\n";
    }
};

class ASTFunctionPrototype : public ASTNode {
    std::string identifier;
    std::vector<ASTNode*> parameterList;
    std::string returnType;

public:
    ASTFunctionPrototype(std::string identifier, std::vector<ASTNode*> parameterList, std::string returnType): ASTNode(AST_FN_PROTOTYPE), identifier(identifier), parameterList(parameterList), returnType(returnType) {}
    ~ASTFunctionPrototype() {
        // delete each node the vector
        for (ASTNode *parameter : parameterList) {
            delete parameter;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Prototype:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        if (!parameterList.empty()) {
            std::cout << std::string((depth + 1) * 2, ' ') << "Parameters:\n";
            // print each node the vector
            for (ASTNode *parameter : parameterList) {
                parameter->print((depth + 1) + 1);
            }
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Parameters\n";
        }
        // check if string is not empty
        if (!returnType.empty())
            std::cout << std::string((depth + 1) * 2, ' ') << "Return type: " << returnType << "\n";
        else
            std::cout << std::string((depth + 1) * 2, ' ') << "No Return Type\n";
    }
};

class ASTFunctionDefinition : public ASTNode {
    ASTNode *prototype;
    ASTNode *body;

public:
    ASTFunctionDefinition(ASTNode *prototype, ASTNode *body): ASTNode(AST_FN_DEFINITION), prototype(prototype), body(body) {}
    ~ASTFunctionDefinition() {
        delete prototype;
        delete body;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Definition:\n";
        prototype->print(depth + 1);
        body->print(depth + 1);
    }
};

class ASTReturnStatement : public ASTNode {
    ASTNode *expression;

public:
    explicit ASTReturnStatement(ASTNode *expression): ASTNode(AST_RETURN_STATEMENT), expression(expression) {}
    ~ASTReturnStatement() {
        delete expression;
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Return Statement:\n";
        expression->print(depth + 1);
    }
};

class ASTFunctionCall : public ASTNode {
    std::string identifier;
    std::vector<ASTNode*> argumentList;

public:
    ASTFunctionCall(std::string identifier, std::vector<ASTNode*> argumentList): ASTNode(AST_FN_CALL), identifier(identifier), argumentList(argumentList) {}
    ~ASTFunctionCall() {
        // delete each node the vector
        for (ASTNode *argument : argumentList) {
            delete argument;
        }
    }

    void print(int depth) const override {
        std::cout << std::string(depth * 2, ' ') << "Function Call:\n";
        std::cout << std::string((depth + 1) * 2, ' ') << "Identifier: " << identifier << "\n";
        if (!argumentList.empty()) {
            std::cout << std::string((depth + 1) * 2, ' ') << "Arguments\n";
            // print each node the vector
            for (ASTNode *argument : argumentList) {
                argument->print(depth + 2);
            }
        } else {
            std::cout << std::string((depth + 1) * 2, ' ') << "No Arguments\n";
        }
    }
};
