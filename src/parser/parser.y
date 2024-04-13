/* Copyright 2024 Lucas Norman */

%{
#include <iostream>
#include <string>
#include <memory>
#include <chrono>

#include "./ast.h"
#include "./wrappers.h" // for VectorWrapper and StringWrapper

#include "../diagnostics/parser.h"

extern int yylex();
extern int yyparse();
extern FILE *yyin;
extern int yylineno;
void yyerror(const char *s);
std::unique_ptr<AST> ast;
%}

%union {
    int int_literal;
    float float_literal;
    struct StringWrapper *str;
    char character;
    struct VectorWrapper *nodeList;
    class ASTNode *node;
}

%token<int_literal> TOK_INT_LITERAL
%token<float_literal> TOK_FLOAT_LITERAL
%token<str> TOK_IDENTIFIER TOK_TYPE TOK_STR_LITERAL TOK_F_STR_START TOK_F_STR_MIDDLE TOK_F_STR_END
%token<character> TOK_CHAR_LITERAL
%token TOK_LET TOK_EQUALS TOK_SEMICOLON TOK_PLUS TOK_HYPHEN TOK_ASTERISK TOK_F_SLASH TOK_L_PAREN TOK_R_PAREN TOK_L_BRACE TOK_R_BRACE TOK_RETURN TOK_IF TOK_WHILE TOK_FN TOK_COMMA TOK_ARROW TOK_COLON TOK_DOT

%type<node> expression instanceStatement localStatement compoundStatement statement functionDefinition functionPrototype parameter returnStatement functionCall numericalExpression term factor
%type<nodeList> instanceStatementList statementList parameterList argumentList

%start program

/* bison grammar */
%%
program
    : instanceStatementList {ast = std::make_unique<AST>($1->getVector()); delete $1;}
    ;

instanceStatementList
    : instanceStatementList instanceStatement {$$ = $1; $$->push($2);}
    | instanceStatement {$$ = new VectorWrapper($1);}
    ;

statementList
    : statementList statement {$$ = $1; $$->push($2);}
    | statement {$$ = new VectorWrapper($1);}
    ;

statement
    : instanceStatement {$$ = $1;}
    | localStatement {$$ = $1;}
    ;

instanceStatement
    : functionDefinition {$$ = $1;}
    ;

localStatement
    : expression TOK_SEMICOLON {$$ = $1;}
    | returnStatement TOK_SEMICOLON {$$ = $1;}
    | compoundStatement {$$ = $1;}
    ;

compoundStatement
    : TOK_L_BRACE statementList TOK_R_BRACE {$$ = new ASTCompoundStatement($2->getVector()); delete $2;}
    | TOK_L_BRACE TOK_R_BRACE {$$ = new ASTCompoundStatement();}
    ;

functionDefinition
    : functionPrototype compoundStatement {$$ = new ASTFunctionDefinition($1, $2);}
    ;

functionPrototype
    : TOK_FN TOK_IDENTIFIER TOK_L_PAREN parameterList TOK_R_PAREN {$$ = new ASTFunctionPrototype($2->getString(), $4->getVector(), ""); delete $2; delete $4;}
    | TOK_FN TOK_IDENTIFIER TOK_L_PAREN parameterList TOK_R_PAREN TOK_ARROW TOK_TYPE {$$ = new ASTFunctionPrototype($2->getString(), $4->getVector(), $7->getString()); delete $2; delete $4; delete $7;}
    ;

returnStatement
    : TOK_RETURN expression {$$ = new ASTReturnStatement($2);}
    ;

parameterList
    : parameterList TOK_COMMA parameter {$$ = $1; $$->push($3);}
    | parameter {$$ = new VectorWrapper($1);}
    | {$$ = new VectorWrapper();}
    ;

parameter
    : TOK_IDENTIFIER TOK_COLON TOK_TYPE {$$ = new ASTParameter($1->getString(), $3->getString()); delete $1; delete $3;}
    ;

functionCall
    : TOK_IDENTIFIER TOK_L_PAREN argumentList TOK_R_PAREN {$$ = new ASTFunctionCall($1->getString(), $3->getVector()); delete $1; delete $3;}
    ;

argumentList
    : argumentList TOK_COMMA expression {$$ = $1; $$->push($3);}
    | expression {$$ = new VectorWrapper($1);}
    | {$$ = new VectorWrapper();}
    ;

expression
    : numericalExpression {$$ = $1;}
    ;

numericalExpression
    : numericalExpression TOK_PLUS term {$$ = new ASTBinaryOperator($1, $3, '+');}
    | numericalExpression TOK_HYPHEN term {$$ = new ASTBinaryOperator($1, $3, '-');}
    | term {$$ = $1;}
    ;

term
    : term TOK_ASTERISK factor {$$ = new ASTBinaryOperator($1, $3, '*');}
    | term TOK_F_SLASH factor {$$ = new ASTBinaryOperator($1, $3, '/');}
    | factor {$$ = $1;}
    ;

factor
    : TOK_INT_LITERAL {$$ = new ASTNumber($1);}
    | TOK_HYPHEN TOK_INT_LITERAL {$$ = new ASTNumber(-$2);} /* To handle cases like: (-5) * 2; and 3 * -2; */
    | TOK_L_PAREN expression TOK_R_PAREN {$$ = $2;}
    | TOK_IDENTIFIER {$$ = new ASTVariableExpression($1->getString()); delete $1;}
    | functionCall {$$ = $1;}
    ;
%%

// runs when bison finds an error
void yyerror(const char *s) {
    parser::fatal_error(std::chrono::high_resolution_clock::now(), s, "A parsing error occurred on line: " + std::to_string(yylineno));
}

std::unique_ptr<AST> &parse(const std::string& filename) {
    FILE *file = fopen(filename.c_str(), "r");
    // yyin is the file that bison will read from
    yyin = file;

    // creates the ast
    yyparse();
    return ast;
}
