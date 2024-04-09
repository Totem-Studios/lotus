%{
#include <cstdlib> // for std::atoi
#include <iostream>
#include "../parser/parser.tab.h"
#include "../parser/wrappers.h" // for StringWrapper
%}

%option yylineno

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
(return|ret) {return TOK_RETURN;}
"if" {return TOK_IF;}
"while" {return TOK_WHILE;}
"fn" {return TOK_FN;}
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
{string} {yylval.str = new StringWrapper(yytext); return TOK_STR_LITERAL;}
{multi_line_comment} {/* count the number of newlines */ for (char c : std::string(yytext)) if (c == '\n') yylineno++;}
{one_line_comment} {}
[ \t\r] {}
\n {yylineno++;}
. {std::cout << "Unrecognized input: " << yytext << "\n";}
%%

// flex uses this when it encounters end of file, to check if it should continue to another file
int yywrap() {
    // 1 means terminate and 0 means continue
    return 1;
}
