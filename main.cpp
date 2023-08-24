//
// Created by neoma on 2023-08-21.
//

#include <iostream>
#include <fstream>

#include "resources/config.h"
#include "resources/token.h"
#include "resources/node.h"

class Preprocessor {
    typedef struct {
        std::string macro, value;
    } definition;
    std::vector<definition> definitions;
    std::vector<char> all_unique_utils;
    static std::string get_use(char buffer[], std::streamsize *index) {
        std::string usage_directory;
        while(!isalnum(buffer[*index])) {
            if(!isspace(buffer[*index])) {
                std::cerr << "\nsyntax error: 'use' directive expects spacing: ' ', and then a utility starting with an alpha character (letter), not: " << buffer[*index] << "\n";
            }
            ++*index;
        }
        while(buffer[*index] != EOF && (isalnum(buffer[*index]) || buffer[*index] == '_')) {
            usage_directory+=buffer[*index];
            ++*index;
        }

        if(usage_directory == "interface") {
            return interface;
        } else if(usage_directory == "filestream") {
            return filestream;
        } else if(usage_directory == "time") {
            return time;
        } else if(usage_directory == "threads") {
            return threads;
        } else if(usage_directory == "async") {
            return async;
        } else if(usage_directory == "math") {
            return math;
        } else {
            std::cerr << "builtin utility undefined\n";
            exit(1);
        }
    }

    static definition get_def(char buffer[], std::streamsize *index) {
        definition def;
        while(!isalnum(buffer[*index])) {
            if(!isspace(buffer[*index])) {
                std::cerr << "syntax error: 'def' directive expects spacing: ' ', and then a definition starting with an alpha character (letter), not: " << buffer[*index] << "\n";
            }
            ++*index;
        }
        while(buffer[*index] != '\n' && (isalnum(buffer[*index]) || buffer[*index] == '_')) {
            def.macro+=buffer[*index];
            ++*index;
        }

        while(!isalnum(buffer[*index])) {
            if(!isspace(buffer[*index])) {
                std::cerr << "syntax error: when defining a macro the macro should be followed by a spacing: ' ', later a alpha character (letter), not: " << buffer[*index] << "\n";
            }
            ++*index;
        }
        while(!isspace(buffer[*index])) {
            def.value+=buffer[*index];
            ++*index;
        }
        return def;
    }

    static std::string get_selective_use(char buffer[], std::streamsize *index) {
        std::string selective_part;
        while(isalnum(buffer[*index]) || buffer[*index] == '_') {
            selective_part+=buffer[*index];
            ++index;
        }
        return selective_part;
    }

    void process(char buffer[], std::streamsize& bSize) {
        for(std::streamsize i = 0; i < bSize; ++i) {
            if(buffer[i] == '/') {
                ++i;
                if(buffer[i] == '/') {
                    while(buffer[i] != '\n') ++i;
                } else if(buffer[i] == '*') {
                    ++i;
                    while(buffer[i] != '*' && buffer[i+1] != '/') ++i;
                } else {
                    content.push_back('/');
                    content.push_back(buffer[i]);
                }
            } else if(buffer[i] == '@') {
                ++i;
                while(isspace(buffer[i])) ++i;
                if(!isalpha(buffer[i])) {
                    std::cerr << "syntax error: undefined syntax for preprocessor-directive-statement" << std::endl;
                } else {
                    std::string directive;
                    while(isalpha(buffer[i])) directive+=buffer[i++];
                    if(directive == "use") {
                        std::ifstream usage_directory(get_use(buffer, &i), std::ios::binary);
                        if(!usage_directory) {std::cerr << "failed to open utility directory"; exit(1);}
                        while(isspace(buffer[i]) && buffer[i] != '\n' && buffer[i] != ':') ++i;
                        if(buffer[i] == '\n') {
                            added_file_rows++;
                            while(usage_directory.peek() != EOF) {
                                if(usage_directory.peek() == '\n') added_file_rows++;
                                content.push_back(static_cast<char>(usage_directory.get()));
                            }
                            content.push_back('\n');
                            added_file_rows++;
                        } else if(buffer[i] == ':'){
                            get_selective_use(buffer, &i);
                        } else {
                            std::cerr << "syntax error: utility can only be followed by a colon: ':'\n";
                        }
                    } else if(directive == "seize") {

                    } else if(directive == "banish") {

                    } else if(directive == "force") {

                    } else if(directive == "def") {
                        definition def = get_def(buffer, &i);
                        bool isDefined = false;
                        for(const definition& d : definitions) {
                            if(d.macro == def.macro && d.value == def.value) {
                                std::cerr << "syntax error: definition of macro encountered > 1 time(s): @def" << def.macro << " " << def.value << "\n";
                                isDefined = true;
                            }
                        }
                        if(!isDefined) definitions.emplace_back(def);
                    } else {
                        std::cerr << "undefined preprocessor-directive" << std::endl;
                        exit(1);
                    }
                }
            } else if(buffer[i] == '#') {

            } else {
                content.push_back(buffer[i]);
            }
        }
    }
public:
    size_t added_file_rows = 0;
    std::vector<char> content;
    explicit Preprocessor(std::string& directory) {
        std::ifstream stream(directory, std::ios::binary | std::ios::ate);
        std::streamsize pFileSize = stream.tellg();
        char buffer[pFileSize];
        stream.seekg(0, std::ios::beg);
        stream.read(buffer, pFileSize);
        stream.close();

        process(buffer, pFileSize);

        for(const char& c : content) {
            std::cout << c;
        }
    }

    ~Preprocessor() = default;
};

class Lexer {
public:
    std::vector<token> tokens;
    explicit Lexer(const std::vector<char>& content) {
        unsigned int row = 1, index = 1;
        std::string lexeme;
        for(size_t i = 0; i < content.size(); i++) {
            if(isspace(content[i])) {
                if(content[i] == '\n') {
                    row++;
                    index = 1;
                }
                continue;
            } else if(isalpha(content[i])) {
                while(isalnum(content[i]) || content[i] == '_') {
                    lexeme+=content[i++];
                }
                i--;
                tokens.emplace_back(token{match_token(lexeme), lexeme});
                lexeme.clear();
            } else if (isdigit(content[i])) {
                while(isdigit(content[i])) {
                    lexeme+=content[i];
                    i++;
                }
                i--;
                if(isspace(content[i])) {
                    tokens.emplace_back(token{TOK_INTEGER_LIT, lexeme});
                    lexeme.clear();
                } else if(content[i+1] == '.' && isdigit(content[i+2])) {
                    lexeme+=content[++i]; i++;
                    while(isdigit(content[i])) {
                        lexeme+=content[i];
                        i++;
                    }
                    i--;
                    tokens.emplace_back(token{TOK_RATIONAL_LIT, lexeme});
                    lexeme.clear();
                } else if(content[i+1] == '.' && !isdigit(content[i+2])) {
                    std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                              << "\trational literal: " << lexeme << ". cannot terminate with alpha character (letter), must be digit\n";
                    tokens.emplace_back(token{TOK_INTEGER_LIT, lexeme});
                    lexeme.clear();
                } else if(isalpha(content[i+1])) {
                    std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                              << "\tinteger literal: " << lexeme << " cannot terminate with alpha character (letter)\n";
                    tokens.emplace_back(token{TOK_INTEGER_LIT, lexeme});
                    lexeme.clear();
                } else {
                    tokens.emplace_back(token{TOK_INTEGER_LIT, lexeme});
                    lexeme.clear();
                }
            } else if(isprint(content[i])) {
                if(content[i] == '(') {
                    tokens.emplace_back(token{TOK_LPAREN, "("});
                } else if(content[i] == ')') {
                    tokens.emplace_back(token{TOK_RPAREN, ")"});
                } else if(content[i] == '{') {
                    tokens.emplace_back(token{TOK_RBRACE, "{"});
                } else if(content[i] == '}') {
                    tokens.emplace_back(token{TOK_LBRACE, "}"});
                } else if(content[i] == '[') {
                    tokens.emplace_back(token{TOK_LBRACKET, "["});
                } else if(content[i] == ']') {
                    tokens.emplace_back(token{TOK_RBRACKET, "]"});
                } else if(content[i] == '.') {
                    if(content[i+1] == '.' && content[i+2] == '.') {
                        tokens.emplace_back(token{TOK_SEQUENCE_SPECIFIER, "..."});
                        i+=2;
                    } else {
                        tokens.emplace_back(token{TOK_DOT, "."});
                    }
                } else if(content[i] == ',') {
                    tokens.emplace_back(token{TOK_COMMA, ","});
                } else if(content[i] == ':') {
                    if(content[i+1] == ':') {
                        tokens.emplace_back(token{TOK_NAMESPACE_ACC_SPECIFIER, "::"});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_COLON, ":"});
                    }
                } else if(content[i] == ';') {
                    tokens.emplace_back(token{TOK_SEMICOLON, ";"});
                } else if(content[i] == '\"') {
                    lexeme+="\""; i++;
                    while(i < content.size() && content[i] != '\"') {
                        if(content[i] == '\\') {
                            lexeme+=content[i++];
                        }
                        lexeme+=content[i++];
                    }
                    lexeme+=content[i];
                    if(i >= content.size()) {
                        std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                                  << "\tunclosed string literal: " << lexeme << "\n";
                    }
                    tokens.emplace_back(token{TOK_STR_LIT, lexeme});
                    lexeme.clear();
                } else if(content[i] == '\'') {
                    lexeme+="\'"; i++;
                    while(i < content.size() && content[i] != '\'') {
                        if(content[i] == '\\') {
                            lexeme+=content[i++];
                        }
                        lexeme+=content[i++];
                    }
                    lexeme+=content[i];

                    if(lexeme.length() > 4) {
                        if(i >= content.size()) {
                            std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                                      << "\tunclosed string literal: " << lexeme << "\n";
                            tokens.emplace_back(token{TOK_STR_LIT, lexeme});
                            lexeme.clear();
                        } else {
                            tokens.emplace_back(token{TOK_STR_LIT, lexeme});
                            lexeme.clear();
                        }
                    } else {
                        if(i >= content.size()) {
                            std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                                      << "\tunclosed character literal: " << lexeme << "\n";
                            tokens.emplace_back(token{TOK_CHAR_LIT, lexeme});
                            lexeme.clear();
                        } else {
                            tokens.emplace_back(token{TOK_CHAR_LIT, lexeme});
                            lexeme.clear();
                        }
                    }

                } else if(content[i] == '_') {
                    while(isalnum(content[i]) || content[i] == '_') {
                        lexeme+=content[i++];
                    }
                    i--;
                    tokens.emplace_back(token{match_token(lexeme), lexeme});
                    lexeme.clear();
                } else if(content[i] == '=') {
                    if(content[i+1] == '=') {
                        tokens.emplace_back(token{TOK_LOGICAL_COMP, "=="});
                        i++;
                    } else if(content[i+1] == '=' && content[i+2] == '=') {
                        tokens.emplace_back(token{TOK_LOGICAL_FULLCOMP, "==="});
                        i+=2;
                    } else {
                        tokens.emplace_back(token{TOK_EQ, "="});
                    }
                } else if(content[i] == '+') {
                    if(content[i+1] == '+') {
                        tokens.emplace_back(token{TOK_INC1_OP, "++"});
                        i++;
                    } else if(content[i+1] == '=') {
                        tokens.emplace_back(token{TOK_INC_OP, "+="});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_PLUS, "+"});
                    }
                } else if(content[i] == '-') {
                    if(content[i+1] == '>') {
                        tokens.emplace_back(token{TOK_MEMBER_DEREFERENCE_OP, "->"});
                        i++;
                    } else if(content[i+1] == '-') {
                        tokens.emplace_back(token{TOK_DEC1_OP, "--"});
                        i++;
                    } else if(content[i+1] == '=') {
                        tokens.emplace_back(token{TOK_DEC_OP, "-="});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_MINUS, "-"});
                    }
                } else if(content[i] == '*') {
                    if(content[i+1] == '*') {
                        tokens.emplace_back(token{TOK_SQR_OP, "**"});
                        i++;
                    } else if(content[i+1] == '=') {
                        tokens.emplace_back(token{TOK_MUL_OP, "*="});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_ASTRIKS, "*"});
                    }
                } else if(content[i] == '|') {
                    if(content[i+1] == '|') {
                        tokens.emplace_back(token{TOK_LOGIC_OR_OP, "||"});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_OR_OP, "|"});
                    }
                } else if(content[i] == '^') {
                    tokens.emplace_back(token{TOK_SQR_OP, "^"});
                } else if(content[i] == '~') {
                    tokens.emplace_back(token{TOK_DESTRUCTOR_SPECIFIER, "~"});
                } else if(content[i] == '&') {
                    if (content[i + 1] == '&') {
                        tokens.emplace_back(token{TOK_LOGIC_AND_OP, "&&"});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_REFERENCE, "&"});
                    }
                } else if(content[i] == '!') {
                    if (content[i + 1] == '=') {
                        tokens.emplace_back(token{TOK_LOGIC_NOT_COMP, "!="});
                        i++;
                    } else if (content[i + 1] == '=' && content[i + 2] == '=') {
                        tokens.emplace_back(token{TOK_LOGIC_NOT_FULLCOMP, "!=="});
                        i+=2;
                    } else {
                        tokens.emplace_back(token{TOK_LOGIC_NOT, "!"});
                    }
                } else if(content[i] == '<') {
                    if (content[i + 1] == '=') {
                        tokens.emplace_back(token{TOK_SMALLER_THAN_COMP, "<="});
                        i++;
                    } else if (content[i + 1] == '=' && content[i + 2] == '=') {
                        tokens.emplace_back(token{TOK_SMALLER_THAN_FULLCOMP, "<=="});
                        i+=2;
                    } else if (content[i + 1] == '<') {
                        tokens.emplace_back(token{TOK_LOGICAL_BITSHIFT_L, "<<"});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_SMALLER_THAN, "<"});
                    }
                } else if(content[i] == '>') {
                    if (content[i + 1] == '=') {
                        tokens.emplace_back(token{TOK_BIGGER_THAN_COMP, ">="});
                        i++;
                    } else if (content[i + 1] == '=' && content[i + 2] == '=') {
                        tokens.emplace_back(token{TOK_BIGGER_THAN_FULLCOMP, ">=="});
                        i+=2;
                    } else if (content[i + 1] == '>') {
                        tokens.emplace_back(token{TOK_LOGICAL_BITSHIFT_R, ">>"});
                        i++;
                    } else {
                        tokens.emplace_back(token{TOK_BIGGER_THAN, ">"});
                    }
                } else {
                    tokens.emplace_back(token{TOK_UNKNOWN, std::to_string(content[i])});
                    //WARNING: this character is unknown
                }
            } else {
                std::cerr << "\nsyntax error: " << row << "|" << index << "\n"
                          << "\tunprocessable character: " << content[i++] << "\n";
            }
        }
    }
    ~Lexer() = default;
};

class Parser {
    static std::vector<node> parse(const std::vector<token>& tokens) {
        std::vector<node> program;
        for(size_t i = 0; i < tokens.size(); i++) {

        }
        return program;
    }
public:
    node root;
    explicit Parser(const std::vector<token>& tokens) {
        root.type = NODE_ROOT;
        root.children = parse(tokens);
    }

    ~Parser() = default;
};

class Semantic_Analyzer {
    static void analyze(const node& root) {
        std::cout << "\n[program]:\n\n";
        //Semantics below ->
    }
public:
    explicit Semantic_Analyzer(const node& root) {
        analyze(root);
    }
};

int main() {
    std::string directory = "code.pri";
    // std::cin >> directory;
    Preprocessor preprocessor(directory);
    Lexer lexer(preprocessor.content);

    std::cout << "\n\n\n";
    for(const token& t : lexer.tokens) {
        //std::cout << "{" << t.type << ", " << t.lexeme << "}" << "\n";
        if(std::to_string(t.type).length() == 2) {
            std::cout << t.type << " |  " << t.lexeme << "\n";
        } else {
            std::cout << t.type << "  |  " << t.lexeme << "\n";
        }
    }

    Parser parser(lexer.tokens);
    Semantic_Analyzer semantic_analyzer(parser.root);
    return 0;
}