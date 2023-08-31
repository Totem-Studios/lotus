#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <chrono>

#include "./resources/diagnostics.h"

class Preprocessor {
    typedef struct {
        std::string macro, value;
    } macro;

    std::vector<std::string> utilities;
    std::vector<macro> macros;

    static std::string get_directive(std::ifstream& stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream.get();
        std::string directive;
        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') {
            stream.get();
        }
        if(stream.peek() == '\n' || stream.peek() == '\r') {
            diagnostics::preprocessor_error(time, "meaningless '@'", "| no directive was stated after '@'", "");
        } else if(!isalpha(stream.peek())) {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when interpreting directive (digit/symbol): ", std::to_string(static_cast<char>(stream.peek())));
            while(stream.peek() != '\n') stream.get();
        } else {

            while(stream.peek() != EOF && isalpha(stream.peek())) directive+=static_cast<char>(stream.get());

        }
        return directive;
    }

    static std::string get_utility(std::ifstream& stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream.get();
        std::string utility;
        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') {
            std::cerr << static_cast<char>(stream.get());
        }
        if(stream.peek() == '\n' || stream.peek() == '\r') {
            diagnostics::preprocessor_error(time,"meaningless '@use'","| no utility was stated after '@use'", "");
            exit(1);
        } /* else if(!isalpha(stream.peek())) {
            std::cerr << "lotus ~ preprocessor error: syntax error\n\t| encountered illegal character when interpreting utility (digit/symbol): " << static_cast<char>(stream.peek()) << "\n\t| ~ compilation terminated" << std::endl;
            exit(1);
        } */
        else {
            while(stream.peek() != EOF && stream.peek() != '\n' && stream.peek() != '\r'/*isalpha(stream.peek())*/) utility+=static_cast<char>(stream.get());
        }
        return utility;
    }

    static macro get_macro(std::ifstream& stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream.get();
        macro this_macro;
        while(stream.peek() != EOF && !isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') stream.get();
        if(stream.peek() == '\r' || stream.peek() == '\n') {
            diagnostics::preprocessor_error(time,"missing macro","| no useful macro was stated after '@def'", "");
        } else if(!isalpha(stream.peek()) && stream.peek() != '_') {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when defining macro (digit/symbol): ", std::to_string(static_cast<char>(stream.peek())));
        } else {
            while(isalnum(stream.peek()) || stream.peek() == '_') {
                this_macro.macro+=static_cast<char>(stream.get());

            }
        }

        if(stream.peek() == '\r' || stream.peek() == '\n') {
            diagnostics::preprocessor_error(time, "missing value", "| no value was given to macro: ", this_macro.macro);
            return this_macro;
        } else if(stream.peek() != ' ' && stream.peek() != '\t') {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when defining macro value (digit/symbol): ", std::to_string(static_cast<char>(stream.peek())));
            return this_macro;
        }

        while(stream.peek() != EOF && !isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\r' && stream.peek() != '\n') stream.get();
        if(stream.peek() == '\r' || stream.peek() == '\n') {
            diagnostics::preprocessor_error(time, "missing value", "| no value was given to macro: ", this_macro.macro);
            return this_macro;
        } else {
            while(stream.peek() != EOF && stream.peek() != '\n' && stream.peek() != '\r') {
                this_macro.value+=static_cast<char>(stream.get());
            }
        }
        return this_macro;
    }

    std::string check_macro(const std::string& pos_macro, const std::chrono::high_resolution_clock::time_point& time) {
        std::string this_macro = pos_macro;
        for(const macro& m : macros ) {
            if(m.macro == pos_macro) {
                this_macro = m.value;
            }
        }
        return this_macro;
    }

    void include_utility_files(const std::string& directory, const std::chrono::high_resolution_clock::time_point& time) {
        std::ifstream stream(directory, std::ios::binary);
        if(!stream) {
            diagnostics::internal_error(time,"filestream error", "| failed to open directory: ", directory);
            return;
        }
        while(stream.peek() != EOF) {
            if(stream.peek() == '@') {
                const std::string directive = get_directive(stream, time);
                if(directive == "use") {
                    const std::string utility = get_utility(stream, time);
                    include_utility_files(utility, time);
                } else if(directive == "seize") {

                } else if(directive == "banish") {

                } else if(directive == "force") {

                } else {
                    content.push_back('@');
                    for(const char& c : directive) content.push_back(c);
                }
            } else {
                content.push_back(static_cast<char>(stream.get()));
            }
        }
    }

    std::string get_directive_from_content(size_t *i, const std::chrono::high_resolution_clock::time_point& time) {
        ++*i;
        std::string directive;
        while(!isalpha(content[*i]) && (content[*i] == ' ' || content[*i] == '\t') && content[*i] != '\n') {
            ++*i;
        }
        if(content[*i] == '\n' || content[*i] == '\r') {
            diagnostics::preprocessor_error(time, "meaningless '@'", "| no directive was stated after '@'", "");
        } else if(!isalpha(content[*i])) {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when interpreting directive (digit/symbol): ", std::to_string(static_cast<char>(content[*i])));
            while(content[*i] != '\n') ++*i;
        } else {
            while(content[*i] != EOF && isalpha(content[*i])) directive+=static_cast<char>(content[(*i)++]);
        }
        return directive;
    }

    macro get_macro_from_content(size_t *i, const std::chrono::high_resolution_clock::time_point& time) {
        ++*i;
        macro this_macro;
        while(content[*i] != EOF && !isalpha(content[*i]) && (content[*i] == ' ' || content[*i] == '\t') && content[*i] != '\n') ++*i;
        if(content[*i] == '\r' || content[*i] == '\n') {
            diagnostics::preprocessor_error(time,"missing macro","| no useful macro was stated after '@def'", "");
        } else if(!isalpha(content[*i]) && content[*i] != '_') {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when defining macro (digit/symbol): ", std::to_string(static_cast<char>(content[*i])));
        } else {
            while(isalnum(content[*i]) || content[*i] == '_') {
                this_macro.macro+=static_cast<char>(content[(*i)++]);
            }
        }

        if(content[*i] == '\r' || content[*i] == '\n') {
            diagnostics::preprocessor_error(time, "missing value", "| no value was given to macro: ", this_macro.macro);
            return this_macro;
        } else if(content[*i] != ' ' && content[*i] != '\t') {
            diagnostics::syntax_error(time, "illegal character", "| encountered illegal character when defining macro value (digit/symbol): ", std::to_string(static_cast<char>(content[*i])));
            return this_macro;
        }

        while(content[*i] != EOF && !isalpha(content[*i]) && (content[*i] == ' ' || content[*i] == '\t') && content[*i] != '\r' && content[*i] != '\n') ++*i;
        if(content[*i] == '\r' || content[*i] == '\n') {
            diagnostics::preprocessor_error(time, "missing value", "| no value was given to macro: ", this_macro.macro);
            return this_macro;
        } else {
            while(content[*i] != EOF && content[*i] != '\n' && content[*i] != '\r') {
                this_macro.value+=static_cast<char>(content[(*i)++]);
            }
        }
        return this_macro;
    }

    void test_conditionals(const std::chrono::high_resolution_clock::time_point& time) {
        std::vector<char> updated_content;
        for(size_t i = 0; i < content.size(); ++i) {
            if(content[i] == '@') {
                const std::string directive = get_directive_from_content(&i, time);
                if(directive == "def") {
                    macros.push_back(get_macro_from_content(&i, time));
                } else if(directive == "if") {
                    // Test conditional
                } else if(directive == "ifdef") {
                    // Test conditional
                } else if(directive == "ifndef") {
                    // Test conditional
                } else if(directive == "elif") {
                    // Test conditional
                } else if(directive == "endif") {
                    // Terminate conditional chain
                } else {
                    // Handle error
                }
            } else {
                updated_content.push_back(content[i]);
            }
        }
        content = updated_content;
    }
public:
    std::vector<char> content;
    std::string lexer_ready_content;
    explicit Preprocessor(const std::string& directory, const std::chrono::high_resolution_clock::time_point& time) {
        include_utility_files(directory ,time);
        /*
        for(const std::string& util : utilities) {
            std::cout << "utility: " << util << std::endl;
        }
        std::cout << std::endl;
        for(const macro& m : macros) {
            std::cout << "defined: " << m.macro << " " << m.value << std::endl;
        }
        std::cout << std::endl;
        for(const char& c : content) {
            std::cout << c;
        }
         */
        for(const char& c : content) {
            std::cout << c;
        }
        std::cout << "\n\n\n";
        test_conditionals(time);
        for(const char& c : content) {
            std::cout << c;
        }
        std::cout << "\n\n\n";
        const auto end_time = std::chrono::high_resolution_clock::now();
        const auto prep_time = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - time);
        std::cout << "\n\npreprocessor finished after " << prep_time.count() << " ms" << std::endl;
    }
};

class Lexer {
    enum tok_type {
        TOK_ID,

        TOK_INTEGER_LIT,
        TOK_RATIONAL_LIT,
    };
    typedef struct {
        tok_type type;
        std::string lexeme;
    } token;
public:
    std::vector<token> tokens;
    explicit Lexer(std::vector<char>& content) {
        unsigned int row = 1, index = 1;
        std::string lexeme;
        for(size_t i = 0; i < content.size(); ++i) {
            if(isspace(content[i])) {
                if(content[i] == '\n') {
                    index = 1;
                    row++;
                } else if(content[i] == ' ') {
                    index++;
                }
            } else if(isdigit(content[i])) {
                while(isdigit(content[i])) lexeme+=content[i++];
                if(isalpha(content[i])) {
                    std::cerr << "syntax error:\t\ninteger literal must end with space, not a letter!\n";
                    tokens.push_back(token{TOK_INTEGER_LIT, lexeme}); lexeme.clear(); continue;
                } else if(content[i] == '.' && isdigit(content[i+1])) {
                    lexeme+=".";
                    while(isdigit(content[i])) lexeme+=content[++i];
                    tokens.push_back(token{TOK_RATIONAL_LIT, lexeme}); lexeme.clear(); continue;
                } else if(content[i] == '.' && !isdigit(content[i+1])) {
                    lexeme+=".";
                    std::cerr << "syntax error:\t\nrational literal must end with space, not a letter!\n";
                    tokens.push_back(token{TOK_RATIONAL_LIT, lexeme}); lexeme.clear(); continue;
                } else {
                    tokens.push_back(token{TOK_INTEGER_LIT, lexeme}); lexeme.clear(); continue;
                }
            } else if(isalpha(content[i])) {
                while(isalnum(content[i]) || content[i] == '_') lexeme+=content[i++];
                // tokens.push_back(token{match_keyword(lexeme), lexeme}); lexeme.clear(); continue;
            } else if(isprint(content[i])) {
                continue;
            } else {
                continue;
            }
        }
    }
};

void get_command() {
    std::string command_line = "code.lts";
    //std::cin >> command_line;
    // std::getline(std::cin, command_line);
    // GET COMMAND HERE
    const auto time = std::chrono::high_resolution_clock::now();
    Preprocessor preprocessor(command_line, time);
}

int main() {
    get_command();
    return 0;
}