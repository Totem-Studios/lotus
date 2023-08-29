#include <iostream>
#include <fstream>
#include <vector>

class Preprocessor {
    typedef struct {
        std::string macro, value;
    } macro;

    std::vector<std::string> utilities;
    std::vector<macro> macros;

    std::string get_directive(std::ifstream& stream) {
        stream.get();
        std::string directive;
        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') {
            stream.get();
        }
        if(stream.peek() == '\n' || stream.peek() == '\r') {
            std::cerr << "lotus ~ preprocessor error: meaningless '@'\n\t| no directive was stated after '@'\n\t| ~ compilation terminated" << std::endl;
        } else if(!isalpha(stream.peek())) {
            std::cerr << "lotus ~ preprocessor error: syntax error\n\t| encountered illegal character when interpreting directive (digit/symbol): " << static_cast<char>(stream.peek()) << "\n\t| ~ compilation terminated" << std::endl;
            while(stream.peek() != '\n') stream.get();
        } else {
            while(isalpha(stream.peek())) directive+=static_cast<char>(stream.get());
        }
        return directive;
    }

    std::string get_utility(std::ifstream& stream) {
        stream.get();
        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') {
            std::cerr << static_cast<char>(stream.get());
        }
        if(stream.peek() == '\n' || stream.peek() == '\r') {
            std::cerr << "lotus ~ preprocessor error: meaningless '@use'\n\t| no utility was stated after '@use'\n\t| ~ compilation terminated" << std::endl;
            exit(1);
        } /* else if(!isalpha(stream.peek())) {
            std::cerr << "lotus ~ preprocessor error: syntax error\n\t| encountered illegal character when interpreting utility (digit/symbol): " << static_cast<char>(stream.peek()) << "\n\t| ~ compilation terminated" << std::endl;
            exit(1);
        } */ else {
            std::string utility;
            while(stream.peek() != '\n' && stream.peek() != '\r'/*isalpha(stream.peek())*/) utility+=static_cast<char>(stream.get());
            return utility;
        }
    }

    macro get_macro(std::ifstream& stream) {
        stream.get();
        macro this_macro;
        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') stream.get();
        if(stream.peek() == '\r' || stream.peek() == '\n') {
            std::cerr << "~syntax error~\n\tno useful macro was stated after '@def'" << "\n";
        } else if(!isalpha(stream.peek()) && stream.peek() != '_') {
            std::cerr << "~syntax error~\n\tencountered illegal character when defining macro (digit/symbol): " << stream.peek() << "\n";
        } else {
            while(isalnum(stream.peek()) || stream.peek() == '_') {
                this_macro.macro+=static_cast<char>(stream.get());

            }
        }

        if(stream.peek() == '\r' || stream.peek() == '\n') {
            std::cerr << "~syntax error~\n\tmacro " << this_macro.macro << " has no definition\n";
            return this_macro;
        } else if(stream.peek() != ' ' && stream.peek() != '\t') {
            std::cerr << "~syntax error~\n\tencountered illegal character when defining macro value (digit/symbol): ";
            return this_macro;
        }

        while(!isalpha(stream.peek()) && (stream.peek() == ' ' || stream.peek() == '\t') && stream.peek() != '\n') stream.get();
        if(stream.peek() == '\r' || stream.peek() == '\n') {
            std::cerr << "~syntax error~\n\tno value was given to macro: " << this_macro.macro << "\n";
            return this_macro;
        } else {
            while(stream.peek() != '\n' && stream.peek() != '\r') {
                this_macro.value+=static_cast<char>(stream.get());
            }
        }
        return this_macro;
    }

    std::string check_macro(const std::string& pos_macro) {
        std::string this_macro = pos_macro;
        for(const macro& m : macros ) {
            if(m.macro == pos_macro) {
                this_macro = m.value;
            }
        }
        return this_macro;
    }

    void process(const std::string& directory) {
        std::ifstream stream(directory, std::ios::binary | std::ios::ate);
        if(!stream) {
            std::cerr << "lotus ~ internal error: filestream error\n\t| failed to open directory: " << directory << "\n\t| ~ process terminated" << std::endl;
            return;
        }
        size_t fileSize = stream.tellg();
        stream.seekg(0, std::ios::beg);
        bool isSeized = false;
        while(stream.peek() != EOF) {
            if(stream.peek() == '@') {
                const std::string directive = get_directive(stream);
                if(directive == "use") {
                    const std::string utility = get_utility(stream);
                    if(utility == directory) {
                        std::cerr << "lotus ~ preprocessor error: filestream error\n\t| directory includes itself: "
                                  << directory
                                  << "\n\t| ~ process terminated" << std::endl;
                        return;
                    }
                    if(isSeized) {
                        utilities.push_back(utility);
                    }
                    for(const std::string& util : utilities) {
                        if(util == utility) {

                        } else if(util == directory) {

                        } else if(util == utility && util == directory) {

                        }
                    }
                    if(utility[utility.size()-1] == ':') {
                        std::cerr << "NOT IMPLEMENTED YET!\n";
                    } else {
                        process(utility);
                    }
                } else if(directive == "seize") {
                    isSeized = true;
                } else if(directive == "banish") {

                } else if(directive == "def") {
                    macros.push_back(get_macro(stream));
                } else if(directive == "if") {

                } else if(directive == "ifdef") {

                } else if(directive == "ifndef") {

                } else if(directive == "endif") {

                } else {
                    std::cerr << "lotus ~ preprocessor error: directive error\n\t| failed to deduce directive: " << directive << "\n\t| ~ process terminated" << std::endl;
                }
            } else {
                if(isalpha(stream.peek()) || stream.peek() == '_') {
                    std::string pos_macro;
                    while(isalpha(stream.peek()) || stream.peek() == '_') {
                        pos_macro+=static_cast<char>(stream.get());
                    }
                    std::string result = check_macro(pos_macro);
                    for(const char& c : result) {
                        content.push_back(c);
                    }
                } else {
                    content.push_back(static_cast<char>(stream.get()));
                }
            }
        }
        stream.close();
    }
public:
    std::vector<char> content;
    explicit Preprocessor(const std::string& directory) {
        process(directory);
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
    Preprocessor preprocessor(command_line);
}

int main() {
    get_command();
    return 0;
}