cmake_minimum_required(VERSION 3.26)
project(Lotus)

set(CMAKE_CXX_STANDARD 23)

# Define the output directory for binaries
set(OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin")

# Specify the directory for the binary outputs (executables and libraries)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${OUTPUT_DIRECTORY})
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${OUTPUT_DIRECTORY})
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${OUTPUT_DIRECTORY})

add_executable(Lotus src/main.cpp
        src/preprocessor/preprocessor.h
        src/preprocessor/preprocessor.cpp
        src/diagnostics/internal/internal.h
        src/diagnostics/common.h
        src/diagnostics/lexer.h
        src/diagnostics/parser.h
        src/diagnostics/preprocessor.h
        src/diagnostics/semantic_analyzer.h
        src/parser/parser.h
        src/parser/parser.cpp
        src/lexer/lexer.h
        src/lexer/lexer.cpp
        src/lexer/token.h
        src/parser/ast/node.h
        src/optimizer/optimizer.cpp
        tests/errors/test_errors.cpp
        tests/lexer/test_lexer.cpp
        tests/parser/test_parser.cpp
        tests/performance/test_performance.cpp
        src/generator/x86_64/generator.h
        src/generator/x86_64/generator.cpp
        tests/preprocessor/test_preprocessor.cpp
)

# set_target_properties(Lotus PROPERTIES OUTPUT_NAME "lotus")
