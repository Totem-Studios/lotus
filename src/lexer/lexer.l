%{
#include <cstdlib> // for std::atoi
#include <iostream>
%}

/* Regex */
type bool|i8|u8|i16|u16|i32|u32|i64|u64|isize|usize|f32|f64|char
identifier [A-Za-z_]+[0-9]*
character ['][^'.][']
integer_literal [0-9]+
float_literal [0-9]*\.[0-9]+
string ["][^".]*["]
multi_line_comment [/][*](\n|.)*[*][/]
one_line_comment ([/][/]).*

/* Rules */
%%
(return|ret) {std::cout << yytext << "\n";}
"if" {std::cout << yytext << "\n";}
"while" {std::cout << yytext << "\n";}
"fn" {std::cout << yytext << "\n";}
"->" {std::cout << yytext << "\n";}
"=" {std::cout << yytext << "\n";}
"+" {std::cout << yytext << "\n";}
"-" {std::cout << yytext << "\n";}
"*" {std::cout << yytext << "\n";}
"/" {std::cout << yytext << "\n";}
"(" {std::cout << yytext << "\n";}
")" {std::cout << yytext << "\n";}
"{" {std::cout << yytext << "\n";}
"}" {std::cout << yytext << "\n";}
";" {std::cout << yytext << "\n";}
":" {std::cout << yytext << "\n";}
"," {std::cout << yytext << "\n";}
"." {std::cout << yytext << "\n";}
{type} {std::cout << yytext << "\n";}
{identifier} {std::cout << yytext << "\n";}
{integer_literal} {std::cout << yytext << "\n";}
{float_literal} {std::cout << yytext << "\n";}
{character} {std::cout << yytext << "\n";}
{string} {std::cout << yytext << "\n";}
{multi_line_comment} {}
{one_line_comment} {}
[ \t\r] {}
\n {}
. {std::cout << "Unrecognized input: " << yytext << "\n";}
%%

// flex uses this when it encounters end of file, to check if it should continue to another file
int yywrap() {
    // 1 means terminate and 0 means continue
    return 1;
}
