<img src="./assets/lotus_logo_text.svg" height="70px" alt="An image of the Lotus logo with the text lotus below" />

# Lotus: The power of simplicity

Lotus is a type-safe compiled programming language designed to simplify the syntax of C++ and achieve memory safety using ownership and borrowing.

The syntax of Lotus is mainly C/C++ based however adding new features and "syntactic-sugar". The goal of the language is a more elegant approach to high-demand programming. In other words, Lotus has a bias for systems programming.

This repository contains the original compiler for Lotus. It is not yet finished and offers minimal debugging support with unoptimized binary. It is incomplete, as can be said for the Lotus SDL.

## Highlights and roadmap of Lotus

- [x] Lexer implementation
- [x] Header extension support
- [x] Visual Studio Code extension
- [x] Debug mode configuration settings
- [x] LLVM integration
- [x] Parser development with bison
- [x] AST - Abstract Syntax Tree
- [ ] Optimized Binary Generation (upcoming)
- [ ] Comprehensive standard library (upcoming)
- [ ] Advanced error handling and diagnostics (upcoming)
- [ ] Multi-language support (upcoming)

## Deep-dive into Lotus

To learn more about Lotus, our detailed documentation is the perfect place to start. You can access it on our GitHub page: [Lotus Documentation](https://github.com/Totem-Studios/docs-website).

The documentation covers everything from basic syntax and language features to advanced topics. Whether you're a beginner or an experienced developer, you'll find valuable information to help you get the most out of Lotus.

## Lotus for Visual Studio Code

To enhance the experience of writing and debugging Lotus code, we've developed a Visual Studio Code extension. You can access it here: [Lotus Language Support](https://github.com/Totem-Studios/LLS-vscode).

## Project structure

| Directory                | Contents                                                                            |
| ------------------------ | ----------------------------------------------------------------------------------- |
| `.github/workflows`      | Contains the workflow files for GitHub Actions to help us test and deploy the code. |
| `.github/ISSUE_TEMPLATE` | Contains the templates used when creating an issue on our GitHub repository.        |
| `assets/`                | Contains branding materials and other non-code assets for the repository.           |
| `bin/`                   | Compiled binary files generated during the build process.                           |
| `config/`                | Configuration files for changing modes and settings.                                |
| `grammar/`               | Grammar specifications for the Lotus programming language.                          |
| `logs/`                  | Execution logs for Lotus programs.                                                  |
| `sdl/`                   | Lotus Standard Library files.                                                       |
| `src/`                   | Core components of the programming language, including the lexer, parser, and more. |
| `tests/`                 | Test files created by the development team to test and verify internal logic.       |

## How to build the project

First off make sure that you have all prerequisites installed on your machine and that your machine supports x86_64 assembly (AMD_64). This can be done in a terminal or command line interface.

## How to run the project

To run a Lotus project, use the `lotus` command followed by the source filename and an optional output filename and destination. This can be done in a terminal or command line interface. Here is the basic command structure:

```shell
# To run a Lotus program, use the `lotus` command followed by the source filename and the optional output filename.
# Replace `main.lts` with the path to your Lotus source file and `output` with your desired output file name.
lotus main.lts output
```
