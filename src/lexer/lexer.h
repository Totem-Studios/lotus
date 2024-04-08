// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <chrono>

#include "./token.h"

#include "../../config/config.h"
#include "../diagnostics/lexer.h"

class Lexer {
    void lex(const std::string *lexer_ready_content, const std::chrono::high_resolution_clock::time_point& time) {
        std::string construct;
        size_t index = 0, pos = 1, row = 1;
        while (index < lexer_ready_content->size()) {
            if (isspace((*lexer_ready_content)[index])) {
                if ((*lexer_ready_content)[index] == '\n') {
                    pos = 1;
                    row++;
                } else {
                    pos++;
                }
            } else if (isdigit((*lexer_ready_content)[index])) {
                unsigned int dotCount = 0;
                while (isdigit((*lexer_ready_content)[index]) || (*lexer_ready_content)[index] == '.') {
                    if ((*lexer_ready_content)[index] == '.') {
                        dotCount++;
                    }
                    construct += (*lexer_ready_content)[index++];
                }
                index--;
                if (dotCount == 0) {
                    tokens.emplace_back(token {TOK_INTEGER_LIT, construct});
                } else if (dotCount == 1) {
                    tokens.emplace_back(token {TOK_RATIONAL_LIT, construct});
                } else {
                    // Syntax error!
                }
                construct.clear();
            } else if (isalpha((*lexer_ready_content)[index]) || (*lexer_ready_content)[index] == '_') {
                if ((*lexer_ready_content)[index] == '_' && (*lexer_ready_content)[index+1] == '/') {
                    index++;
                    tokens.emplace_back(token {TOK_SQRT, "_/"});
                } else {
                    while (isalnum((*lexer_ready_content)[index]) || (*lexer_ready_content)[index] == '_') {
                        construct += (*lexer_ready_content)[index++];
                    }
                    index--;
                    tokens.emplace_back(token {match(construct), construct});
                    construct.clear();
                }
            } else if (isprint((*lexer_ready_content)[index])) {
                switch ((*lexer_ready_content)[index]) {
                    case '.':
                        if ((*lexer_ready_content)[index+1] == '.' && (*lexer_ready_content)[index+2] == '.') {
                            index += 2;
                            tokens.emplace_back(token {TOK_SEQUENCE, "..."});
                        } else {
                            tokens.emplace_back(token {TOK_DOT, "."});
                        }
                        break;
                    case ',':
                        tokens.emplace_back(token {TOK_COMMA, ","});
                        break;
                    case ':':
                        if ((*lexer_ready_content)[index+1] == ':') {
                            index++;
                            tokens.emplace_back(token {TOK_SCOPE, "::"});
                        } else {
                            tokens.emplace_back(token {TOK_COLON, ":"});
                        }
                        break;
                    case ';':
                        tokens.emplace_back(token {TOK_SEMICOLON, ";"});
                        break;
                    case '(':
                        tokens.emplace_back(token {TOK_LPAREN, "("});
                        break;
                    case ')':
                        tokens.emplace_back(token {TOK_RPAREN, ")"});
                        break;
                    case '{':
                        tokens.emplace_back(token {TOK_LBRACE, "{"});
                        break;
                    case '}':
                        tokens.emplace_back(token {TOK_RBRACE, "}"});
                        break;
                    case '[':
                        tokens.emplace_back(token {TOK_LBRACKET, "["});
                        break;
                    case ']':
                        tokens.emplace_back(token {TOK_RBRACKET, "]"});
                        break;
                    case '<':
                        if (isalpha((*lexer_ready_content)[index+1])) {
                            // Pontus can implement
                        } else if ((*lexer_ready_content)[index+1] == '<' && (*lexer_ready_content)[index+2] != '<') {
                            index++;
                            tokens.emplace_back(token {TOK_LBITSHIFT, "<<"});
                        } else if ((*lexer_ready_content)[index+1] == '=' && (*lexer_ready_content)[index+2] != '=') {
                            index++;
                            tokens.emplace_back(token {TOK_LESS_EQUAL, "<="});
                        } else if ((*lexer_ready_content)[index+1] == '=' && (*lexer_ready_content)[index+2] == '=') {
                            index += 2;
                            tokens.emplace_back(token {TOK_UNKNOWN, "<=="});  // Must be given tok_type
                        } else if ((*lexer_ready_content)[index+1] == '-') {
                            index++;
                            tokens.emplace_back(token {TOK_LARROW, "<-"});
                        } else {
                            tokens.emplace_back(token {TOK_LANGLE, "<"});
                        }
                        break;
                    case '>':
                        if ((*lexer_ready_content)[index+1] == '>' && (*lexer_ready_content)[index+2] != '>') {
                            index++;
                            tokens.emplace_back(token{TOK_RBITSHIFT, ">>"});
                        } else if ((*lexer_ready_content)[index+1] == '=' && (*lexer_ready_content)[index+2] != '=') {
                            index++;
                            tokens.emplace_back(token {TOK_MORE_EQUAL, ">="});
                        } else if ((*lexer_ready_content)[index+1] == '=' && (*lexer_ready_content)[index+2] == '=') {
                            index += 2;
                            tokens.emplace_back(token {TOK_UNKNOWN, ">=="});  // Must be given tok_type
                        } else {
                            tokens.emplace_back(token {TOK_RANGLE, ">"});
                        }
                        break;
                    case '|':
                        if ((*lexer_ready_content)[index+1] == '|') {
                            index++;
                            tokens.emplace_back(token {TOK_LOGICAL_OR, "||"});
                        } else {
                            tokens.emplace_back(TOK_PIPE, "|");
                        }
                        break;
                    case '\"':  // Must be optimized
                        construct += (*lexer_ready_content)[index++];
                        do {
                            if ((*lexer_ready_content)[index] == '\\') {
                                construct += (*lexer_ready_content)[index++];
                            }
                            construct += (*lexer_ready_content)[index++];
                        } while ((*lexer_ready_content)[index] != '\"');
                        construct += (*lexer_ready_content)[index++];
                        tokens.emplace_back(token {TOK_STRING_LIT, construct});
                        construct.clear();
                        break;
                    case '\'':  // Must be optimized
                        construct += (*lexer_ready_content)[index++];
                        do {
                            if ((*lexer_ready_content)[index] == '\\') {
                                construct += (*lexer_ready_content)[index++];
                            }
                            construct += (*lexer_ready_content)[index++];
                        } while ((*lexer_ready_content)[index] != '\'');
                        construct += (*lexer_ready_content)[index++];
                        tokens.emplace_back(token {TOK_CHAR_LIT, construct});
                        construct.clear();
                        break;
                    case '+':
                        if ((*lexer_ready_content)[index+1] == '+') {
                            index++;
                            tokens.emplace_back(token {TOK_INCREMENT, "++"});
                        } else if ((*lexer_ready_content)[index+1] == '=') {
                            index++;
                            tokens.emplace_back(token {TOK_PLUS_EQUAL, "+="});
                        } else {
                            tokens.emplace_back(token {TOK_PLUS, "+"});
                        }
                        break;
                    case '-':
                        if ((*lexer_ready_content)[index+1] == '-') {
                            index++;
                            tokens.emplace_back(token {TOK_DECREMENT, "--"});
                        } else if ((*lexer_ready_content)[index+1] == '=') {
                            index++;
                            tokens.emplace_back(token {TOK_MINUS_EQUAL, "-="});
                        } else if ((*lexer_ready_content)[index+1] == '>') {
                            index++;
                            tokens.emplace_back(token {TOK_RARROW, "->"});
                        } else {
                            tokens.emplace_back(token {TOK_HYPHEN, "-"});
                        }
                        break;
                    case '*':
                        tokens.emplace_back(token {TOK_ASTERISK, "*"});
                        break;
                    case '/':
                        if ((*lexer_ready_content)[index + 1] == '/') {
                            index++;
                            while (!(index < lexer_ready_content->size() && (*lexer_ready_content)[index] == '\r' && (*lexer_ready_content)[index + 1] == '\n')) index++;
                        } else if ((*lexer_ready_content)[index + 1] == '*') {
                            index += 2;
                            std::string comment = "/*";
                            while (index < lexer_ready_content->size() && !((*lexer_ready_content)[index] == '*' && (*lexer_ready_content)[index + 1] == '/')) {
                                comment += (*lexer_ready_content)[index++];
                            }
                            comment += "*/";
                            if (index >= lexer_ready_content->size())
                                syntax::fatal_error(time, "unclosed multi-line comment", "| encountered unclosed multi-line comment while lexing: ", comment);
                            else
                                index++;
                        } else if ((*lexer_ready_content)[index + 1] == '=') {
                            index++;
                            tokens.emplace_back(token {TOK_FSLASH_EQUAL, "/="});
                        } else {
                            tokens.emplace_back(token {TOK_FSLASH, "/"});
                        }
                        break;
                    case '=':
                        if ((*lexer_ready_content)[index+1] == '=') {
                            index++;
                            tokens.emplace_back(token {TOK_DOUBLE_EQUAL, "=="});
                        } else if ((*lexer_ready_content)[index+1] == '=' && (*lexer_ready_content)[index + 2] == '=') {
                            index += 2;
                            tokens.emplace_back(token {TOK_TRIPLE_EQUAL, "==="});
                        } else {
                            tokens.emplace_back(token {TOK_EQUAL, "="});
                        }
                        break;
                    case '?':
                        tokens.emplace_back(TOK_QUESTIONMARK, "?");
                        break;
                    case '&':
                        if ((*lexer_ready_content)[index+1] == '&') {
                            index++;
                            tokens.emplace_back(token {TOK_LOGICAL_AND, "&&"});
                        } else {
                            tokens.emplace_back(TOK_AMPERSAND, "&");
                        }
                        break;
                    default:
                        // Syntax error! Any symbol not defined above is not supported
                        tokens.emplace_back(TOK_UNKNOWN, "");
                        syntax::nonfatal_error(time, "unsupported symbol", "| encountered unsupported or undefined character while lexing: ", std::string(1, static_cast<char>((*lexer_ready_content)[index])));
                        break;
                }
            } else {
                // Syntax error!
            }
            // REMEMBER TO REMOVE LATER (OR KEEP DEPENDING ON LOGIC)
            index++;
        }
        tokens.emplace_back(TOK_END_OF_FILE);
    }

 public:
    std::vector<token> tokens;
    explicit Lexer(const std::string *lexer_ready_content, const std::chrono::high_resolution_clock::time_point& time) {
        lex(lexer_ready_content, time);
        const auto end_time = std::chrono::high_resolution_clock::now();
        const auto prep_time = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - time);
        std::cout << "\n\nlexer finished after " << prep_time.count() << " ms" << std::endl;
        // For the moment: displays all tokens
        for (const token& t : tokens) {
            std::cout << "{" << t.type << ", " << t.lexeme << "}\n";
        }
    }

    ~Lexer() = default;
};
