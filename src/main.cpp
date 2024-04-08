// Copyright 2023 Pontus Henriksson & Neo Mannskär

#include <iostream>
#include <chrono>

#include "./preprocessor/preprocessor.h"
#include "./lexer/lexer.h"
#include "./parser/parser.h"
#include "./analyzer/semantic_analyzer.h"
#include "./generator/x86_64/generator.h"

extern int yylex();
extern FILE *yyin;

int main(int argc, char** argv) {
    if (argc < 2) {std::cerr << "Usage: " << argv[0] << " filename.lts" << std::endl; return 1;}

    FILE *file = fopen("code.lts", "r");
    yyin = file;
    return yylex();

    return 0;
    std::string compilable = /*"../tests/code.lts"*/argv[1];
    const auto time = std::chrono::high_resolution_clock::now();
    Preprocessor preprocessor(compilable, time);
    Lexer lexer(&preprocessor.content, time);
    Parser parser(&lexer.tokens, time);
    SemanticAnalyzer semanticAnalyzer(parser.root, time);
    Generator generator(semanticAnalyzer.root, time);
    if (argc == 3)
        generator.fillAsmFile(argv[2]);
    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto durTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime-time);
    std::cout << "\n\ncompilation successful: " << durTime.count() << "ms" << std::endl;
    return 0;
}
