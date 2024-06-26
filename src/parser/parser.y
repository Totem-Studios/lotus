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
%token TOK_LET TOK_EQUALS TOK_SEMICOLON TOK_PLUS TOK_HYPHEN TOK_ASTERISK TOK_F_SLASH TOK_L_PAREN TOK_R_PAREN TOK_L_BRACE TOK_R_BRACE TOK_RETURN TOK_IF TOK_ELSE TOK_WHILE TOK_FN TOK_COMMA TOK_ARROW TOK_COLON TOK_DOT TOK_DOUBLE_EQUALS TOK_LESS_THAN TOK_GREATER_THAN TOK_LESS_THAN_EQUALS TOK_GREATER_THAN_EQUALS TOK_NOT_EQUALS TOK_TRUE TOK_FALSE TOK_NOT TOK_AND TOK_OR

%type<node> expression instanceStatement localStatement compoundStatement functionDefinition functionPrototype parameter returnStatement functionCall numericalExpression term factor closedStatement openStatement booleanExpression booleanExpression2 booleanExpression3
%type<nodeList> instanceStatementList localStatementList parameterList argumentList

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

instanceStatement
    : functionDefinition {$$ = $1;}
    ;

localStatementList
    : localStatementList localStatement {$$ = $1; $$->push($2);}
    | localStatement {$$ = new VectorWrapper($1);}
    ;

localStatement
    : closedStatement {$$ = $1;}
    | openStatement {$$ = $1;}
    ;

closedStatement
    : TOK_IF expression compoundStatement TOK_ELSE closedStatement {$$ = new ASTIfElseStatement($2, $3, $5);}
    | TOK_IF expression TOK_COLON closedStatement TOK_ELSE closedStatement {$$ = new ASTIfElseStatement($2, $4, $6);}
    | expression TOK_SEMICOLON {$$ = $1;}
    | returnStatement TOK_SEMICOLON {$$ = $1;}
    | compoundStatement {$$ = $1;}
    ;

openStatement
    : TOK_IF expression compoundStatement {$$ = new ASTIfStatement($2, $3);}
    | TOK_IF expression compoundStatement TOK_ELSE openStatement {$$ = new ASTIfElseStatement($2, $3, $5);}
    | TOK_IF expression TOK_COLON localStatement {$$ = new ASTIfStatement($2, $4);}
    | TOK_IF expression TOK_COLON closedStatement TOK_ELSE openStatement {$$ = new ASTIfElseStatement($2, $4, $6);}
    ;

compoundStatement
    : TOK_L_BRACE localStatementList TOK_R_BRACE {$$ = new ASTCompoundStatement($2->getVector()); delete $2;}
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
    | booleanExpression {$$ = $1;}
    | TOK_STR_LITERAL {$$ = new ASTString($1->getString()); delete $1;}
    ;

booleanExpression
    : booleanExpression TOK_OR booleanExpression2 {$$ = new ASTBinaryOperator($1, $3, "||");}
    | booleanExpression2 {$$ = $1;}
    ;

booleanExpression2
    : booleanExpression2 TOK_AND booleanExpression3 {$$ = new ASTBinaryOperator($1, $3, "&&");}
    | booleanExpression3 {$$ = $1;}
    ;

booleanExpression3
    : TOK_NOT booleanExpression3 {$$ = $$ = new ASTUnaryOperator($2, "!");}
    | TOK_NOT numericalExpression {$$ = $$ = new ASTUnaryOperator($2, "!");}
    | numericalExpression TOK_DOUBLE_EQUALS numericalExpression {$$ = new ASTBinaryOperator($1, $3, "==");}
    | numericalExpression TOK_NOT_EQUALS numericalExpression {$$ = new ASTBinaryOperator($1, $3, "!=");}
    | numericalExpression TOK_LESS_THAN numericalExpression {$$ = new ASTBinaryOperator($1, $3, "<");}
    | numericalExpression TOK_GREATER_THAN numericalExpression {$$ = new ASTBinaryOperator($1, $3, ">");}
    | numericalExpression TOK_LESS_THAN_EQUALS numericalExpression {$$ = new ASTBinaryOperator($1, $3, "<=");}
    | numericalExpression TOK_GREATER_THAN_EQUALS numericalExpression {$$ = new ASTBinaryOperator($1, $3, ">=");}
    | TOK_TRUE {$$ = new ASTNumber(1);}
    | TOK_FALSE {$$ = new ASTNumber(0);}
    ;

numericalExpression
    : numericalExpression TOK_PLUS term {$$ = new ASTBinaryOperator($1, $3, "+");}
    | numericalExpression TOK_HYPHEN term {$$ = new ASTBinaryOperator($1, $3, "-");}
    | term {$$ = $1;}
    ;

term
    : term TOK_ASTERISK factor {$$ = new ASTBinaryOperator($1, $3, "*");}
    | term TOK_F_SLASH factor {$$ = new ASTBinaryOperator($1, $3, "/");}
    | factor {$$ = $1;}
    ;

factor
    : TOK_INT_LITERAL {$$ = new ASTNumber($1);}
    | TOK_HYPHEN factor {$$ = new ASTUnaryOperator($2, "-");} /* To handle cases like: (-5) * 2; and 3 * -2; */
    | TOK_PLUS factor {$$ = new ASTUnaryOperator($2, "+");}
    | TOK_L_PAREN numericalExpression TOK_R_PAREN {$$ = $2;}
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
