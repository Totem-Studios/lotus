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
#define YYERROR_VERBOSE 1
%}

%union {
    uint64_t int_literal;
    double float_literal;
    struct StringWrapper *str;
    char character;
    struct VectorWrapper *nodeList;
    class ASTNode *node;
}

%token<int_literal> TOK_INT_LITERAL
%token<float_literal> TOK_FLOAT_LITERAL
%token<str> TOK_IDENTIFIER TOK_TYPE TOK_STR_LITERAL TOK_F_STR_START TOK_F_STR_MIDDLE TOK_F_STR_END
%token<character> TOK_CHAR_LITERAL
%token TOK_LET TOK_EQUALS TOK_SEMICOLON TOK_PLUS TOK_HYPHEN TOK_ASTERISK TOK_F_SLASH TOK_L_PAREN TOK_R_PAREN TOK_L_BRACE TOK_R_BRACE TOK_RETURN TOK_IF TOK_ELSE TOK_WHILE TOK_FOR TOK_FN TOK_COMMA TOK_ARROW TOK_COLON TOK_DOT TOK_DOUBLE_EQUALS TOK_LESS_THAN TOK_GREATER_THAN TOK_LESS_THAN_EQUALS TOK_GREATER_THAN_EQUALS TOK_NOT_EQUALS TOK_TRUE TOK_FALSE TOK_NOT TOK_AND TOK_OR TOK_TILDA TOK_AS TOK_INCREMENT TOK_DECREMENT TOK_PERCENT TOK_PLUS_EQUALS TOK_HYPHEN_EQUALS TOK_PERCENT_EQUALS TOK_ASTERISK_EQUALS TOK_F_SLASH_EQUALS TOK_BREAK TOK_CONTINUE TOK_AMPERSAND

%type<node> expression instanceStatement localStatement compoundStatement functionDefinition functionPrototype parameter returnStatement functionCall arithmeticExpression term factor closedStatement openStatement booleanExpression booleanExpression2 booleanExpression3 variableDeclaration variableAssignment variableDefinition ifCompatibleStatement whileLoop forLoop factor2
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
    : ifCompatibleStatement {$$ = $1;}
    | variableDeclaration TOK_SEMICOLON
    | variableDefinition TOK_SEMICOLON
    ;

/* this defines statements that are safe to use inside of if-statements with the ':' syntax */
ifCompatibleStatement
    : closedStatement {$$ = $1;}
    | openStatement {$$ = $1;}
    ;

/* this defines statements that are self closing and cannot be interpreted as the start of a bigger statement */
closedStatement
    : TOK_IF expression compoundStatement TOK_ELSE closedStatement {$$ = new ASTIfElseStatement($2, $3, $5);}
    | TOK_IF expression TOK_COLON closedStatement TOK_ELSE closedStatement {$$ = new ASTIfElseStatement($2, $4, $6);}
    | expression TOK_SEMICOLON {$$ = $1;}
    | returnStatement TOK_SEMICOLON {$$ = $1;}
    | whileLoop {$$ = $1;}
    | forLoop {$$ = $1;}
    | compoundStatement {$$ = $1;}
    | TOK_CONTINUE TOK_SEMICOLON {$$ = new ASTContinueStatement();}
    | TOK_BREAK TOK_SEMICOLON {$$ = new ASTBreakStatement();}
    ;

/* this defines statements that are open, meaning they can be the start of a bigger statement or not */
openStatement
    : TOK_IF expression compoundStatement {$$ = new ASTIfStatement($2, $3);}
    | TOK_IF expression compoundStatement TOK_ELSE openStatement {$$ = new ASTIfElseStatement($2, $3, $5);}
    | TOK_IF expression TOK_COLON ifCompatibleStatement {$$ = new ASTIfStatement($2, $4);}
    | TOK_IF expression TOK_COLON closedStatement TOK_ELSE openStatement {$$ = new ASTIfElseStatement($2, $4, $6);}
    ;

compoundStatement
    : TOK_L_BRACE localStatementList TOK_R_BRACE {$$ = new ASTCompoundStatement($2->getVector()); delete $2;}
    | TOK_L_BRACE TOK_R_BRACE {$$ = new ASTCompoundStatement();}
    ;

variableDeclaration
    : TOK_IDENTIFIER TOK_COLON TOK_TYPE {$$ = new ASTVariableDeclaration($1->getString(), $3->getString()); delete $1; delete $3;}
    | TOK_TYPE TOK_IDENTIFIER {$$ = new ASTVariableDeclaration($2->getString(), $1->getString()); delete $1; delete $2;}
    ;

variableAssignment
    : TOK_IDENTIFIER TOK_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, ""); delete $1;}
    | TOK_IDENTIFIER TOK_PLUS_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, "+"); delete $1;}
    | TOK_IDENTIFIER TOK_HYPHEN_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, "-"); delete $1;}
    | TOK_IDENTIFIER TOK_ASTERISK_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, "*"); delete $1;}
    | TOK_IDENTIFIER TOK_F_SLASH_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, "/"); delete $1;}
    | TOK_IDENTIFIER TOK_PERCENT_EQUALS expression {$$ = new ASTVariableAssignment($1->getString(), $3, "%"); delete $1;}
    ;

variableDefinition
    : TOK_IDENTIFIER TOK_COLON TOK_TYPE TOK_EQUALS expression {$$ = new ASTVariableDefinition($1->getString(), $3->getString(), $5); delete $1; delete $3;}
    | TOK_IDENTIFIER TOK_COLON TOK_EQUALS expression {$$ = new ASTVariableDefinition($1->getString(), "auto", $4); delete $1;}
    | TOK_TYPE TOK_IDENTIFIER TOK_EQUALS expression {$$ = new ASTVariableDefinition($2->getString(), $1->getString(), $4); delete $1; delete $2;}
    ;

functionDefinition
    : functionPrototype compoundStatement {$$ = new ASTFunctionDefinition($1, $2);}
    ;

functionPrototype
    : TOK_FN TOK_IDENTIFIER TOK_L_PAREN parameterList TOK_R_PAREN {$$ = new ASTFunctionPrototype($2->getString(), $4->getVector(), ""); delete $2; delete $4;}
    | TOK_FN TOK_IDENTIFIER TOK_L_PAREN parameterList TOK_R_PAREN TOK_ARROW TOK_TYPE {$$ = new ASTFunctionPrototype($2->getString(), $4->getVector(), $7->getString()); delete $2; delete $4; delete $7;}
    ;

whileLoop
    : TOK_WHILE expression compoundStatement {$$ = new ASTWhileLoop($2, $3);}
    ;

forLoop
    : TOK_FOR variableDefinition TOK_SEMICOLON expression TOK_SEMICOLON expression compoundStatement {$$ = new ASTForLoop($2, $4, $6, $7);}
    ;

returnStatement
    : TOK_RETURN expression {$$ = new ASTReturnStatement($2);}
    | TOK_RETURN {$$ = new ASTReturnStatement();}
    ;

parameterList
    : parameterList TOK_COMMA parameter {$$ = $1; $$->push($3);}
    | parameter {$$ = new VectorWrapper($1);}
    | {$$ = new VectorWrapper();}
    ;

parameter
    : TOK_IDENTIFIER TOK_COLON TOK_TYPE {$$ = new ASTParameter($1->getString(), $3->getString()); delete $1; delete $3;}
    | TOK_TYPE TOK_IDENTIFIER {$$ = new ASTParameter($2->getString(), $1->getString()); delete $1; delete $2;}
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
    : booleanExpression {$$ = $1;}
    | variableAssignment {$$ = $1;}
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
    : TOK_NOT booleanExpression3 {$$ = new ASTUnaryOperator($2, "!");}
    | arithmeticExpression TOK_DOUBLE_EQUALS arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, "==");}
    | arithmeticExpression TOK_NOT_EQUALS arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, "!=");}
    | arithmeticExpression TOK_LESS_THAN arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, "<");}
    | arithmeticExpression TOK_GREATER_THAN arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, ">");}
    | arithmeticExpression TOK_LESS_THAN_EQUALS arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, "<=");}
    | arithmeticExpression TOK_GREATER_THAN_EQUALS arithmeticExpression {$$ = new ASTBinaryOperator($1, $3, ">=");}
    | arithmeticExpression {$$ = $1;}
    ;

arithmeticExpression
    : arithmeticExpression TOK_PLUS term {$$ = new ASTBinaryOperator($1, $3, "+");}
    | arithmeticExpression TOK_HYPHEN term {$$ = new ASTBinaryOperator($1, $3, "-");}
    | term {$$ = $1;}
    ;

term
    : term TOK_ASTERISK factor {$$ = new ASTBinaryOperator($1, $3, "*");}
    | term TOK_F_SLASH factor {$$ = new ASTBinaryOperator($1, $3, "/");}
    | term TOK_PERCENT factor {$$ = new ASTBinaryOperator($1, $3, "%");}
    | factor {$$ = $1;}
    ;

factor
    : TOK_HYPHEN factor {$$ = new ASTUnaryOperator($2, "-");} /* To handle cases like: (-5) * 2; and 3 * -2; */
    | TOK_PLUS factor {$$ = new ASTUnaryOperator($2, "+");}
    | factor2
    ;

factor2
    : TOK_INT_LITERAL {$$ = new ASTInteger($1);}
    | TOK_FLOAT_LITERAL {$$ = new ASTFloat($1);}
    | TOK_STR_LITERAL {$$ = new ASTString($1->getString()); delete $1;}
    | TOK_CHAR_LITERAL {$$ = new ASTChar($1);}
    | TOK_TRUE {$$ = new ASTBool(true);}
    | TOK_FALSE {$$ = new ASTBool(false);}
    | TOK_IDENTIFIER {$$ = new ASTVariableExpression($1->getString()); delete $1;}
    | functionCall {$$ = $1;}
    | TOK_IDENTIFIER TOK_INCREMENT {$$ = new ASTIncrementDecrementOperator($1->getString(), "x++"); delete $1;}
    | TOK_IDENTIFIER TOK_DECREMENT {$$ = new ASTIncrementDecrementOperator($1->getString(), "x--"); delete $1;}
    | TOK_INCREMENT TOK_IDENTIFIER {$$ = new ASTIncrementDecrementOperator($2->getString(), "++x"); delete $2;}
    | TOK_DECREMENT TOK_IDENTIFIER {$$ = new ASTIncrementDecrementOperator($2->getString(), "--x"); delete $2;}
    | TOK_AMPERSAND TOK_IDENTIFIER {$$ = new ASTAddressOfOperator($2->getString()); delete $2;}
    | factor2 TOK_AS TOK_TYPE {$$ = new ASTTypeCast($1, $3->getString()); delete $3;}
    | factor2 TOK_TILDA TOK_TYPE {$$ = new ASTTypeCast($1, $3->getString()); delete $3;}
    | TOK_L_PAREN expression TOK_R_PAREN {$$ = $2;}
    ;
%%

// runs when bison finds an error
void yyerror(const char *s) {
    parser::fatal_error(std::chrono::high_resolution_clock::now(), s, "A parsing error occurred on line: " + std::to_string(yylineno));
}

// the function to call to run the bison grammar
std::unique_ptr<AST> &parse(const std::string& filename) {
    FILE *file = fopen(filename.c_str(), "r");
    // yyin is the file that bison will read from
    yyin = file;

    // creates the ast
    yyparse();
    return ast;
}
