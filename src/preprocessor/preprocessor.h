// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include <utility>
#include <vector>
#include <chrono>

#include "../../config/config.h"

#include "../diagnostics/internal/internal.h"
#include "../diagnostics/preprocessor.h"
#include "../diagnostics/lexer.h"

typedef struct {
    std::string macro, value;
} Macro;

class Preprocessor {
    std::string current_directory;
    std::vector<std::string> utilities;
    std::vector<Macro> macros;

    static std::string get_directive(unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        std::string directive;
        while (!isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\n') {
            stream->get();
        }
        if (stream->peek() == '\n' || stream->peek() == '\r') {
            preprocessor::error(time, "meaningless '@'", "| no directive was stated after '@'", "");
            row++;
        } else if (!isalpha(stream->peek())) {
            syntax::error(time, "illegal character", "| encountered illegal character when interpreting directive (digit/symbol): ", std::to_string(static_cast<char>(stream->peek())));
            while (stream->peek() != '\n') stream->get();
            row++;
        } else {
            while (stream->peek() != EOF && isalpha(stream->peek())) directive+=static_cast<char>(stream->get());
        }
        return directive;
    }

    static std::string get_utility(unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        std::string utility;
        while (!isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\n') {
            std::cerr << static_cast<char>(stream->get());
        }
        if (stream->peek() == '\n' || stream->peek() == '\r') {
            preprocessor::error(time, "meaningless '@use'", "| no utility was stated after '@use'", "");
            row++;
            exit(1);
        } else {
            while (stream->peek() != EOF && stream->peek() != '\n' && stream->peek() != '\r') utility+=static_cast<char>(stream->get());
            if (stream->peek() == '\r' || stream->peek() == '\n') row++;
        }
        return utility;
    }

    static Macro get_macro(unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        Macro thisMacro;
        while (stream->peek() != EOF && !isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\n') stream->get();
        if (stream->peek() == '\r' || stream->peek() == '\n') {
            preprocessor::error(time, "missing macro", "| no useful macro was stated after '@def'", "");
            row++;
        } else if (!isalpha(stream->peek()) && stream->peek() != '_') {
            syntax::error(time, "illegal character", "| encountered illegal character when defining macro (digit/symbol): ", std::to_string(static_cast<char>(stream->peek())));
        } else {
            while (isalnum(stream->peek()) || stream->peek() == '_') thisMacro.macro+=static_cast<char>(stream->get());
        }

        if (stream->peek() == '\r' || stream->peek() == '\n') {
            preprocessor::error(time, "missing value", "| no value was given to macro: ", thisMacro.macro);
            row++;
            return thisMacro;
        } else if (stream->peek() != ' ' && stream->peek() != '\t') {
            syntax::error(time, "illegal character", "| encountered illegal character when defining macro value (digit/symbol): ", std::to_string(static_cast<char>(stream->peek())));
            return thisMacro;
        }

        while (stream->peek() != EOF && !isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\r' && stream->peek() != '\n') stream->get();
        if (stream->peek() == '\r' || stream->peek() == '\n') {
            row++;
            preprocessor::error(time, "missing value", "| no value was given to macro: ", thisMacro.macro);
            return thisMacro;
        } else {
            while (stream->peek() != EOF && stream->peek() != '\n' && stream->peek() != '\r') thisMacro.value+=static_cast<char>(stream->get());
            if (stream->peek() == '\r' || stream->peek() == '\n') row++;
        }
        return thisMacro;
    }

    inline bool check_macro(const std::string& pos_macro) {
        for (const Macro& m : macros)
            if (m.macro == pos_macro) return true;
        return false;
    }

    inline bool test_conditional(const std::vector<Macro> *setOfMacros, const std::chrono::high_resolution_clock::time_point& time) {
        // TODO: Implement test_conditional function
        return false;  // default return statement
    }

    bool handle_conditional(std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        Macro thisMacro;
        static std::vector<Macro> setOfMacros;

        while (stream->peek() == ' ' || stream->peek() == '\t') stream->get();
        switch(stream->peek()) {
            case EOF:
                preprocessor::error(time, "incomplete @if", "| no conditionals were stated");
                return false;
            default:
                break;
        }

        if (stream->peek() == '_' || isalpha(stream->peek()))
            while (stream->peek() == '_' || isalnum(stream->peek())) thisMacro.macro+=static_cast<char>(stream->get());
        else {
            preprocessor::error(time, "illegal character", "encountered illegal character when defining macro");
            return false;
        }

        setOfMacros.push_back(thisMacro);

        while (stream->peek() == ' ' || stream->peek() == '\t') stream->get();

        switch(stream->peek()) {
            case EOF:
                preprocessor::error(time, "incomplete @if", "| no conditionals were stated");
                return false;
            default:
                break;
        }

        if (stream->peek() == '\r' || stream->peek() == '\n') {
            return test_conditional(&setOfMacros, time);
        } else {
            handle_conditional(stream, time);
        }

        for (const Macro& m : setOfMacros) {
            std::cout << m.macro << "\n";
        }

        return test_conditional(&setOfMacros, time);
    }

    int handle_use(unsigned int *recursive_calls, unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        if (*recursive_calls >= MAX_UTILITY_RECURSION_DEPTH) {
            preprocessor::error(time, "max recursion-depth reached", "| change codebase structure to have a max recursion depth of: " +  std::to_string(MAX_UTILITY_RECURSION_DEPTH) + " or edit MAX_UTILITY_RECURSION_DEPTH in config");
            return 1;
        } else {
            (*recursive_calls)++;
            utilities.push_back(current_directory);
            std::string directory = get_utility(row, stream, time);
            std::cerr << "!" << directory << "!" << current_directory << "!" << std::endl;
            if (directory == current_directory) {
                preprocessor::error(time, "self-reference in utility file", "| encountered reference to self in '@use' directive in: ", current_directory, false);
                exit(1);
            } else current_directory = directory;
            preprocess(time);
            return 0;
        }
    }

    inline int handle_def(unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        const Macro thisMacro = get_macro(row, stream, time);
        size_t i = 0;
        while (i < macros.size()) {
            if (thisMacro.macro == macros[i].macro) {
                macros[i].value = thisMacro.value;
                i = macros.size() + 1;
            }
            i++;
        }

        if (i <= macros.size())
            macros.push_back(thisMacro);

        return 0;
    }

    int handle_ifdef(bool *isSeized, unsigned int *recursive_calls, unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        std::string this_macro;
        while (stream->peek() != EOF && !isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\n') stream->get();
        if (stream->peek() == '\r' || stream->peek() == '\n') {
            preprocessor::error(time, "missing macro", "| no useful macro was stated after '@ifdef'", "");
            row++;
        } else if (!isalpha(stream->peek()) && stream->peek() != '_') {
            syntax::error(time, "illegal character", "| encountered illegal character when constructing macro (digit/symbol): ", std::to_string(static_cast<char>(stream->peek())));
        } else {
            while (isalnum(stream->peek()) || stream->peek() == '_') this_macro += static_cast<char>(stream->get());
        }

        int result = check_macro(this_macro);
        if (result) {  // THE MACRO IS DEFINED, EXECUTE UNTIL ELIF, ELSE and ENDIF, AND SKIP OVER REST
            while (stream->peek() != EOF) {
                if (stream->peek() == '@') {
                    const std::string directive = get_directive(row, stream, time);
                    if (get_directive(row, stream, time) == "endif") {
                        break;
                    } else if (directive == "elif") {
                        // IMPLEMENT
                        std::cout << "elif!";
                    } else if (directive == "else") {
                        // IMPLEMENT
                        std::cout << "else!";
                    } else {
                        handle_directives(directive, isSeized, recursive_calls, row, stream, time);
                    }
                } else {
                    content.push_back(static_cast<char>(stream->get()));
                }
            }
        } else {  // THE MACRO IS NOT DEFINED, SKIP OVER UNTIL ELIF, ELSE OR ENDIF, AND TEST CONDITIONAL OR EXECUTE OR SKIP
            while (stream->peek() != EOF) {
                if (stream->peek() == '@') {
                    const std::string directive = get_directive(row, stream, time);
                    if (directive == "endif") {
                        break;
                    } else if (directive == "elif") {
                        // IMPLEMENT
                    } else if (directive == "else") {
                        // IMPLEMENT
                    } else {
                        while (stream->peek() != EOF && stream->peek() != '\r' && stream->peek() != '\n') stream->get();
                        if (stream->peek() == EOF) {
                            preprocessor::error(time, "unclosed @ifndef", "| file ended before an '@endif'", "no @endif");
                            row++;
                        }
                    }
                } else {
                    content.push_back(static_cast<char>(stream->get()));
                }
            }
            if (stream->peek() == EOF || stream->peek() == '\r' || stream->peek() == '\n') {
                preprocessor::error(time, "unclosed @ifdef", "| no useful macro was stated after '@ifdef'", "");
                row++;
            }
        }
        return 0;
    }

    int handle_ifndef(bool *isSeized, unsigned int *recursive_calls, unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        stream->get();
        std::string this_macro;
        while (stream->peek() != EOF && !isalpha(stream->peek()) && (stream->peek() == ' ' || stream->peek() == '\t') && stream->peek() != '\n') stream->get();
        if (stream->peek() == '\r' || stream->peek() == '\n') {
            preprocessor::error(time, "missing macro", "| no useful macro was stated after '@ifndef'", "");
            row++;
        } else if (!isalpha(stream->peek()) && stream->peek() != '_') {
            syntax::error(time, "illegal character", "| encountered illegal character when constructing macro (digit/symbol): ", std::to_string(static_cast<char>(stream->peek())));
        } else {
            while (isalnum(stream->peek()) || stream->peek() == '_') this_macro += static_cast<char>(stream->get());
        }

        int result = check_macro(this_macro);
        if (!result) {  // THE MACRO IS NOT DEFINED, EXECUTE UNTIL ELIF, ELSE and ENDIF, AND SKIP OVER REST
            while (stream->peek() != EOF) {
                if (stream->peek() == '@') {
                    const std::string directive = get_directive(row, stream, time);
                    if (get_directive(row, stream, time) == "endif") {
                        std::cerr << "\n\nifndef else endif\n\n";
                        break;
                    } else if (directive == "elif") {
                        // IMPLEMENT
                        std::cout << "elif!";
                    } else if (directive == "else") {
                        // IMPLEMENT
                        std::cout << "else!";
                    } else {
                        handle_directives(directive, isSeized, recursive_calls, row, stream, time);
                    }
                } else {
                    content.push_back(static_cast<char>(stream->get()));
                }
            }
        } else {  // THE MACRO IS DEFINED, SKIP OVER UNTIL ELIF, ELSE OR ENDIF, AND TEST CONDITIONAL OR EXECUTE OR SKIP
            while (stream->peek() != EOF) {
                if (stream->peek() == '@') {
                    const std::string directive = get_directive(row, stream, time);
                    if (directive == "endif") {
                        return 0;  // This means that the ifndef has closed, terminate process.
                    } else if (directive == "elif") {
                        // IMPLEMENT
                        std::cout << "elif!";
                    } else if (directive == "else") {
                        // IMPLEMENT
                        std::cout << "else!";
                    } else {
                        while (stream->peek() != EOF && stream->peek() != '\r' && stream->peek() != '\n') stream->get();
                        if (stream->peek() == EOF) {
                            preprocessor::error(time, "unclosed @ifndef", "| file ended before an '@endif'", "no @endif");
                            row++;
                        }
                    }
                } else {
                    content.push_back(static_cast<char>(stream->get()));
                }
            }
        }
        return 0;
    }

    int handle_directives(const std::string& directive, bool *isSeized, unsigned int *recursive_calls, unsigned int *row, std::ifstream *stream, const std::chrono::high_resolution_clock::time_point& time) {
        if (directive == "use") {
            if (handle_use(recursive_calls, row, stream, time) == 1) return 1;
        } else if (directive == "seize") {
            for (const std::string& s : utilities) {
                if (s == current_directory) {
                    *isSeized = true;
                    return 0;
                }
            }
        } else if (directive == "banish") {
        } else if (directive == "def") {
            return handle_def(row, stream, time);
        } else if (directive == "if") {
            bool result = handle_conditional(stream, time);
            if (result) {
                // SEARCH UNTIL ENDIF AND EXECUTE EVERYTHING THAT IS NOT IN ELIF
            } else if (!result) {
                // SEARCH UNTIL ELIF AND EXECUTE UNTIL ENDIF
            } else {
                // ERROR! ERROR! ERROR!
            }
        } else if (directive == "ifdef") {
            return handle_ifdef(isSeized, recursive_calls, row, stream, time);
        } else if (directive == "ifndef") {
            return handle_ifndef(isSeized, recursive_calls, row, stream, time);
        } else if (directive == "elif") {
            // There is an unclosed conditional: @if, @ifdef, @ifndef
        } else if (directive == "endif") {
            preprocessor::error(time, "missing conditional beginning", "| encountered conditional terminator without any conditional beginning", "");
        } else {
            preprocessor::error(time, "unknown directive", "| encountered encountered unknown preprocessor directive", directive);
        }
        return 0;
    }

    int preprocess(const std::chrono::high_resolution_clock::time_point& time) {
        bool isSeized = false;
        unsigned int row = 1;
        static unsigned int recursive_calls = 0;
        std::ifstream stream(current_directory, std::ios::binary);
        if (!stream) {
            if (recursive_calls == 0) {
                internal::error(time, "filestream.ltsu error", "| failed to open directory: ", current_directory);
                exit(1);
            } else {
                return 0;
            }
        }
        while (stream.peek() != EOF) {
            if (stream.peek() == '@') {
                const std::string directive = get_directive(&row, &stream, time);
                handle_directives(directive, &isSeized, &recursive_calls, &row, &stream, time);
            } else {
                content += static_cast<char>(stream.get());
            }
        }
        stream.close();
        return 0;
    }

 public:
    std::string content;
    explicit Preprocessor(std::string  directory, const std::chrono::high_resolution_clock::time_point& time): current_directory(std::move(directory))  {
        preprocess(time);
        const auto end_time = std::chrono::high_resolution_clock::now();
        const auto prep_time = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - time);
        std::cout << "\n\npreprocessor finished after " << prep_time.count() << " ms" << std::endl;
    }

    ~Preprocessor() = default;
};
