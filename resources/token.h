//
// Created by neoma on 2023-08-21.
//

#pragma once
#include <string>
#include <vector>
#include <unordered_map>

enum tok_type {
    TOK_UNKNOWN = -1,
    TOK_ID,
    TOK_FN,
        TOK_RET,
    TOK_STRUCT,
    TOK_CLASS,
        TOK_ACC_PUB, TOK_ACC_PRI, TOK_ACC_PRO,
    TOK_LOOP, TOK_FOR, TOK_WHILE,
    TOK_CHAR_LIT, TOK_STR_LIT, TOK_DIG_LIT, TOK_NUM_LIT, TOK_DECIMAL_LIT,
    TOK_STATIC_CAST, TOK_DYNAMIC_CAST,
    TOK_REGISTER,
    TOK_LET, TOK_CONST,
    TOK_TYPE_i2, TOK_TYPE_i4, TOK_TYPE_i8, TOK_TYPE_i16, TOK_TYPE_i32,
    TOK_TYPE_si2, TOK_TYPE_si4, TOK_TYPE_si8, TOK_TYPE_si16, TOK_TYPE_si32,
    TOK_TYPE_ui2, TOK_TYPE_ui4, TOK_TYPE_ui8, TOK_TYPE_ui16, TOK_TYPE_ui32,
    TOK_DOT, TOK_COMMA,
    TOK_COLON, TOK_SEMICOLON,
    TOK_LPAREN, TOK_RPAREN,
    TOK_LBRACKET, TOK_RBRACKET,
    TOK_LBRACE, TOK_RBRACE,

    TOK_ARITH_ROOT_OP,
    TOK_SEQUENCE_SPECIFIER,
};

typedef struct {
    tok_type type;
    std::string lexeme;
} token;

tok_type match_token(const std::string& lexeme) {
    std::unordered_map<tok_type, std::string> keywords{
            {TOK_FN,           "fn"},
            {TOK_RET,          "ret"},
            {TOK_STRUCT,       "struct"},
            {TOK_CLASS,        "class"},
            {TOK_ACC_PUB,      "public"},
            {TOK_ACC_PRI,      "private"},
            {TOK_ACC_PRO,      "protected"},
            {TOK_LOOP,         "loop"},
            {TOK_FOR,          "for"},
            {TOK_WHILE,        "while"},
            {TOK_STATIC_CAST,  "static_cast"},
            {TOK_DYNAMIC_CAST, "dynamic_cast"},
            {TOK_REGISTER,     "register"},
            {TOK_LET,          "let"},
            {TOK_CONST,        "const"},
            {TOK_TYPE_i2,      "i2"},
            {TOK_TYPE_i4,      "i4"},
            {TOK_TYPE_i8,      "i8"},
            {TOK_TYPE_i16,     "i16"},
            {TOK_TYPE_i32,     "i32"},
            {TOK_TYPE_si2,     "si2"},
            {TOK_TYPE_si4,     "si4"},
            {TOK_TYPE_si8,     "si8"},
            {TOK_TYPE_si16,    "si16"},
            {TOK_TYPE_si32,    "si32"},
            {TOK_TYPE_ui2,     "ui2"},
            {TOK_TYPE_ui4,     "ui4"},
            {TOK_TYPE_ui8,     "ui8"},
            {TOK_TYPE_ui16,    "ui16"},
            {TOK_TYPE_ui32,    "ui32"},
    };

    for(std::pair<tok_type, std::string> keyword : keywords) {
        if(lexeme == keyword.second) {
            return keyword.first;
        }
    }
    return TOK_ID;
}

std::string print_tokens(std::vector<token>& tokens) {
    return "";
}

/*
 *  {TOK_STR_LIT,      "fn"},
 *  {TOK_CHAR_LIT,     "fn"},
 */