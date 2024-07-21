// Copyright 2024 Pontus Henriksson, Neo Mannskär & Lucas Norman

#include <chrono>
#include <iostream>

#include "./generator/generator.h"
#include "./preprocessor/preprocessor.h"

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " filename.lts" << std::endl;
        return 1;
    }

    const auto time = std::chrono::high_resolution_clock::now();
    Preprocessor preprocessor(argv[1], time);

    const std::string inputFilename = argv[1];
    const std::string outputFilename = argc >= 3 ? argv[2] : "";

    // generate LLVM IR from the input filename and save it to output filename
    // (if present)
    Generator codegen(inputFilename, outputFilename);

    const auto endTime = std::chrono::high_resolution_clock::now();
    const auto durTime =
        std::chrono::duration_cast<std::chrono::milliseconds>(endTime - time);
    std::cout << "\n\ncompilation successful: " << durTime.count() << "ms"
              << std::endl;
    return 0;
}
