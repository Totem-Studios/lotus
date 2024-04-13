// Copyright 2023 Pontus Henriksson & Neo Mannskär, 2024 Lucas Norman

#include <iostream>
#include <chrono>

#include "./preprocessor/preprocessor.h"
#include "./generator/generator.h"

int main(int argc, char** argv) {
    if (argc < 2) {std::cerr << "Usage: " << argv[0] << " filename.lts" << std::endl; return 1;}

    const auto time = std::chrono::high_resolution_clock::now();
    Preprocessor preprocessor(argv[1], time);

    Generator codegen(argv[1]);

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto durTime = std::chrono::duration_cast<std::chrono::milliseconds>(endTime-time);
    std::cout << "\n\ncompilation successful: " << durTime.count() << "ms" << std::endl;
    return 0;
}
