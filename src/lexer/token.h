// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <string>
#include <unordered_map>

enum tokType {
    TOK_END_OF_FILE = -1,
    TOK_IDENTIFIER,
    TOK_FN,
    TOK_CLASS,
    TOK_STRUCT,

    TOK_INTEGER_LIT,
    TOK_RATIONAL_LIT,
    TOK_STRING_LIT,
    TOK_CHAR_LIT,
    TOK_UNKNOWN,

    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_LBRACKET,
    TOK_RBRACKET,
    TOK_LANGLE,    // '<'
    TOK_RANGLE,    // '>'
    TOK_PIPE,
    TOK_AMPERSAND,
    TOK_COMMA,
    TOK_DOT,
    TOK_SEQUENCE,  // ...

    TOK_SEMICOLON,
    TOK_COLON,
    TOK_SCOPE,  // ::
    TOK_QUESTIONMARK,
    TOK_EXCLAMATIONMARK,
    TOK_SINGLE_QUOTE,
    TOK_DOUBLE_QUOTE,

    TOK_LARROW,
    TOK_RARROW,

    TOK_LBITSHIFT,
    TOK_RBITSHIFT,

    TOK_LESS_EQUAL,
    TOK_MORE_EQUAL,
    TOK_EQUAL,
    TOK_DOUBLE_EQUAL,
    TOK_TRIPLE_EQUAL,

    TOK_PLUS_EQUAL,
    TOK_MINUS_EQUAL,
    TOK_ASTERISK_EQUAL,
    TOK_FSLASH_EQUAL,

    TOK_PLUS,
    TOK_HYPHEN,
    TOK_ASTERISK,
    TOK_MODULO,
    TOK_FSLASH,
    TOK_BSLASH,

    TOK_SQRT,
    TOK_SQUARE,
    TOK_INCREMENT,
    TOK_DECREMENT,

    TOK_LOGICAL_OR,
    TOK_LOGICAL_AND,

    TOK_LET,
    TOK_CONST,

    TOK_INT_8,
    TOK_INT_16,
    TOK_INT_32,
    TOK_INT_64,
    TOK_INT_128,

    TOK_UINT_8,
    TOK_UINT_16,
    TOK_UINT_32,
    TOK_UINT_64,
    TOK_UINT_128,

    TOK_SINT_8,
    TOK_SINT_16,
    TOK_SINT_32,
    TOK_SINT_64,
    TOK_SINT_128,

    TOK_FLOAT_8,
    TOK_FLOAT_16,
    TOK_FLOAT_32,
    TOK_FLOAT_64,
    TOK_FLOAT_128,

    TOK_UFLOAT_8,
    TOK_UFLOAT_16,
    TOK_UFLOAT_32,
    TOK_UFLOAT_64,
    TOK_UFLOAT_128,

    TOK_SFLOAT_8,
    TOK_SFLOAT_16,
    TOK_SFLOAT_32,
    TOK_SFLOAT_64,
    TOK_SFLOAT_128,

    TOK_BOOL,
};

typedef struct {
    tokType type;
    std::string lexeme;
} token;

static tokType match(const std::string& construct) {
    // IMPLEMENT HERE
    static std::unordered_map<std::string, tokType> keywords = {
            {"\0", TOK_END_OF_FILE},
            {"fn", TOK_FN},
            {"class", TOK_CLASS},
            {"struct", TOK_STRUCT},

            {"(", TOK_LPAREN},
            {")", TOK_RPAREN},
            {"{", TOK_LBRACE},
            {"}", TOK_RBRACE},
            {"[", TOK_LBRACKET},
            {"]", TOK_RBRACKET},
            {"<", TOK_LANGLE},
            {">", TOK_RANGLE},
            {"|", TOK_PIPE},
            {"&", TOK_AMPERSAND},
            {",", TOK_COMMA},
            {".", TOK_DOT},
            {"...", TOK_SEQUENCE},

            {";", TOK_SEMICOLON},
            {":", TOK_COLON},
            {"::", TOK_SCOPE},
            {"?", TOK_QUESTIONMARK},
            {"!", TOK_EXCLAMATIONMARK},
            {"\'", TOK_SINGLE_QUOTE},
            {"\"", TOK_DOUBLE_QUOTE},

            {"<-", TOK_LARROW},
            {"->", TOK_RARROW},

            {"<<", TOK_LBITSHIFT},
            {">>", TOK_RBITSHIFT},

            {"<=", TOK_LESS_EQUAL},
            {"=>", TOK_MORE_EQUAL},
            {"=", TOK_EQUAL},
            {"==", TOK_DOUBLE_EQUAL},
            {"===", TOK_TRIPLE_EQUAL},

            {"+=", TOK_PLUS_EQUAL},
            {"-=", TOK_MINUS_EQUAL},
            {"*=", TOK_ASTERISK_EQUAL},
            {"/=", TOK_FSLASH_EQUAL},

            {"+", TOK_PLUS},
            {"-", TOK_HYPHEN},
            {"*", TOK_ASTERISK},
            {"%", TOK_MODULO},
            {"/", TOK_FSLASH},
            {"\\", TOK_BSLASH},

            {"_/", TOK_SQRT},
            {"**", TOK_SQUARE},
            {"++", TOK_INCREMENT},
            {"--", TOK_DECREMENT},

            {"||", TOK_LOGICAL_OR},
            {"&&", TOK_LOGICAL_AND},

            {"let", TOK_LET},
            {"const", TOK_CONST},

            {"i8", TOK_INT_8},
            {"i16", TOK_INT_16},
            {"i32", TOK_INT_32},
            {"i64", TOK_INT_64},
            {"i128", TOK_INT_128},

            {"ui8", TOK_UINT_8},
            {"ui16", TOK_UINT_16},
            {"ui32", TOK_UINT_32},
            {"ui64", TOK_UINT_64},
            {"ui128", TOK_UINT_128},

            {"si8", TOK_SINT_8},
            {"si16", TOK_SINT_16},
            {"si32", TOK_SINT_32},
            {"si64", TOK_SINT_64},
            {"si128", TOK_SINT_128},

            {"f8", TOK_FLOAT_8},
            {"f16", TOK_FLOAT_16},
            {"f32", TOK_FLOAT_32},
            {"f64", TOK_FLOAT_64},
            {"f128", TOK_FLOAT_128},

            {"uf8", TOK_UFLOAT_8},
            {"uf16", TOK_UFLOAT_16},
            {"uf32", TOK_UFLOAT_32},
            {"uf64", TOK_UFLOAT_64},
            {"uf128", TOK_UFLOAT_128},

            {"sf8", TOK_SFLOAT_8},
            {"sf16", TOK_SFLOAT_16},
            {"sf32", TOK_SFLOAT_32},
            {"sf64", TOK_SFLOAT_64},
            {"sf128", TOK_SFLOAT_128},

            {"bool", TOK_BOOL},
    };

    for (const auto& p : keywords) {
        if (construct == p.first) {
            return p.second;
        }
    }
    return TOK_IDENTIFIER;
}
