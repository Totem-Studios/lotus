/* Copyright 2024 Lucas Norman */

%{
#include <cstdlib> // for std::atoi
#include <iostream>
#include <chrono>

#include "../parser/parser.tab.h"
#include "../parser/wrappers.h" // for StringWrapper

#include "../diagnostics/lexer.h"
%}

%option yylineno

/* Regex */
type bool|i8|u8|i16|u16|i32|u32|i64|u64|isize|usize|f32|f64|char|auto|str
identifier [A-Za-z_]+[0-9]*
character ['][^'.][']
integer_literal [0-9]+
float_literal [0-9]*\.[0-9]+
string ["][^".]*["]
multi_line_comment [/][*](\n|.)*[*][/]
one_line_comment ([/][/]).*
not not|[!]
and and|[&][&]
or or|[|][|]

/* Rules */
%%
(return|ret) {return TOK_RETURN;}
"if" {return TOK_IF;}
"else" {return TOK_ELSE;}
"while" {return TOK_WHILE;}
"while" {return TOK_FOR;}
"fn" {return TOK_FN;}
"true" {return TOK_TRUE;}
"false" {return TOK_FALSE;}
{not} {return TOK_NOT;}
{and} {return TOK_AND;}
{or} {return TOK_OR;}
"~" {return TOK_TILDA;}
"as" {return TOK_AS;}
"==" {return TOK_DOUBLE_EQUALS;}
"!=" {return TOK_NOT_EQUALS;}
"<=" {return TOK_LESS_THAN_EQUALS;}
">=" {return TOK_GREATER_THAN_EQUALS;}
"<" {return TOK_LESS_THAN;}
">" {return TOK_GREATER_THAN;}
"->" {return TOK_ARROW;}
"=" {return TOK_EQUALS;}
"+" {return TOK_PLUS;}
"-" {return TOK_HYPHEN;}
"*" {return TOK_ASTERISK;}
"/" {return TOK_F_SLASH;}
"(" {return TOK_L_PAREN;}
")" {return TOK_R_PAREN;}
"{" {return TOK_L_BRACE;}
"}" {return TOK_R_BRACE;}
";" {return TOK_SEMICOLON;}
":" {return TOK_COLON;}
"," {return TOK_COMMA;}
"." {return TOK_DOT;}
{type} {yylval.str = new StringWrapper(yytext); return TOK_TYPE;}
{identifier} {yylval.str = new StringWrapper(yytext); return TOK_IDENTIFIER;}
{integer_literal} {yylval.int_literal = std::atoi(yytext); return TOK_INT_LITERAL;}
{float_literal} {yylval.float_literal = std::atoi(yytext); return TOK_FLOAT_LITERAL;}
{character} {yylval.character = yytext[1]; return TOK_CHAR_LITERAL;}
{string} {yylval.str = new StringWrapper(yytext + 1, yyleng - 2); return TOK_STR_LITERAL;}
{multi_line_comment} {}
{one_line_comment} {}
[ \t\r\n] {}
. {syntax::nonfatal_error(std::chrono::high_resolution_clock::now(), "Unrecognized token", "The following characters could not be defined as a token: ", yytext);}
%%

// flex uses this when it encounters end of file, to check if it should continue to another file
int yywrap() {
    // 1 means terminate and 0 means continue
    return 1;
}
