//
// Created by neoma on 2023-08-21.
//

#include <iostream>
#include <fstream>

#include "resources/config.h"
#include "resources/token.h"

class Preprocessor {
    std::string get_use(char buffer[], std::streamsize *index) {
        std::string usage_directory;
        while(!isalnum(buffer[*index])) {
            if(!isspace(buffer[*index])) {
                std::cerr << "syntax error: 'use' directive expects spacing: ' ', and then a utility starting with an alpha character (letter), not: " << buffer[*index] << "\n";
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

    std::string get_selective_use(char buffer[], std::streamsize *index) {
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
                            while(usage_directory.peek() != EOF) {
                                content.push_back(static_cast<char>(usage_directory.get()));
                            }
                            content.push_back('\n');
                        } else if(buffer[i] == ':'){
                            get_selective_use(buffer, &i);
                        } else {
                            std::cerr << "syntax error: utility can only be followed by a colon: ':'\n";
                        }
                    } else if(directive == "seize") {

                    } else if(directive == "banish") {

                    } else if(directive == "force") {

                    } else if(directive == "def") {

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
};

class Lexer {
public:
    std::vector<token> tokens;
    explicit Lexer(const std::vector<char>& content) {
        unsigned int row = 1, index = 1;
        std::string lexeme;
        for(size_t i = 0; i < content.size(); ++i) {
            if(isspace(content[i])) {
                if(content[i] == '\n') {
                    row++;
                    index = 1;
                }
                continue;
            } else if(isalpha(content[i])) {
                lexeme+=content[i];
                while(isalnum(content[i+1]) || content[i+1] == '_') {
                    i++;
                    lexeme+=content[i];
                }
                tokens.emplace_back(token{match_token(lexeme), lexeme});
                lexeme.clear();
            } else if (isdigit(content[i])) {
                lexeme += content[i++];
                while (isdigit(content[i])) {
                    lexeme += content[i++];
                }
                if (content[i] == '.' && isdigit(content[i + 1])) {
                    lexeme += content[i++];
                    while (isdigit(content[i])) {
                        lexeme += content[i++];
                    }
                    tokens.emplace_back(token{TOK_DECIMAL_LIT, lexeme});
                } else {
                    if(lexeme.length() > 1) {
                        tokens.emplace_back(token{TOK_NUM_LIT, lexeme});
                    } else {
                        tokens.emplace_back(token{TOK_DIG_LIT, lexeme});
                    }
                }
                lexeme.clear();
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
                    tokens.emplace_back(token{TOK_DOT, "."});
                } else if(content[i] == ',') {
                    tokens.emplace_back(token{TOK_COMMA, ","});
                } else if(content[i] == ':') {
                    tokens.emplace_back(token{TOK_COLON, ":"});
                } else if(content[i] == ';') {
                    tokens.emplace_back(token{TOK_SEMICOLON, ";"});
                }
            } else {
                std::cerr << "syntax error: unprocessable character: " << content[i] << "\n";
            }
        }
        std::cout << "\n\n\n";
        for(const token& t : tokens) {
            //std::cout << "{" << t.type << ", " << t.lexeme << "}" << "\n";
            if(std::to_string(t.type).length() == 2) {
                std::cout << t.type << " | " << t.lexeme << "\n";
            } else {
                std::cout << t.type << "  | " << t.lexeme << "\n";
            }
        }
    }
};

int main() {
    std::string directory = "code.pri";
    // std::cin >> directory;
    Preprocessor preprocessor(directory);
    Lexer lexer(preprocessor.content);
    return 0;
}

/* issprint()
 *
 * if(content[i] == ';') {
                    tokens.emplace_back(token{TOK_SEMICOLON, ";"});
                } else if(content[i] == ':') {
                    tokens.emplace_back(token{TOK_COLON, ":"});
                } else if(content[i] == '.') {
                    lexeme+=".";
                    if(isdigit(content[i+1])) {
                        i++;
                        tokens.emplace_back(token{TOK_DOT, "."});
                        std::cerr << "syntax error: '.' modifier followed by digit: " << content[i] << "\n";
                    } else if(content[i+1] == '.' && content[i+2] == '.') {
                        i+=2;
                        tokens.emplace_back(token{TOK_SEQUENCE_SPECIFIER, "..."});
                    } else {
                        tokens.emplace_back(token{TOK_DOT, "."});
                    }
                    lexeme.clear();
                } else if(content[i] == ',') {
                    tokens.emplace_back(token{TOK_COMMA, ","});
                    lexeme.clear();
                } else if(content[i] == '(') {
                    tokens.emplace_back(token{TOK_LPAREN, "("});
                    lexeme.clear();
                } else if(content[i] == ')') {
                    tokens.emplace_back(token{TOK_RPAREN, ")"});
                    lexeme.clear();
                } else if(content[i] == '[') {
                    tokens.emplace_back(token{TOK_LBRACKET, "["});
                    lexeme.clear();
                } else if(content[i] == ']') {
                    tokens.emplace_back(token{TOK_RBRACKET, "]"});
                    lexeme.clear();
                } else if(content[i] == '{') {
                    tokens.emplace_back(token{TOK_LBRACE, "{"});
                    lexeme.clear();
                } else if(content[i] == '}') {
                    tokens.emplace_back(token{TOK_RBRACE, "}"});
                    lexeme.clear();
                } else {
                    tokens.emplace_back(token{TOK_COMMA, std::string(content[i], 1)});
                    lexeme.clear();
                }
 *
 *
 */
