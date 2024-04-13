<img src="https://breakerlabs.github.io/docs/media/images/logos/lotus_logo_text.png" height="70px;" alt="An image of the Lotus logo"/>

# Lotus: The Power of Simplicity

Lotus is a type-safe compiled programming language designed to simplify the syntax of C++ and achieve memory safety using ownership and borrowing.

The syntax of Lotus is mainly C/C++ based however adding new features and "syntactic-sugar". The goal of the language is a more elegant approach to high-demand programming. In other words, Lotus has a bias for systems programming.

This repository contains the original compiler for Lotus. It is not yet finished and offers minimal debugging support with unoptimized binary. It is incomplete, as can be said for the Lotus SDL.

| **Platform** | **Architecture** | **Status** |
| ------------ |------------------| ---------- |
| **Windows**  | x86_64           | Underway   |
| **macOS**    | x86_64, ARM      | Upcoming   |
| **Linux**    | x86_64           | Upcoming   |
| **Phone**    | x86_64, ARM      | Upcoming   |

## Highlights and roadmap of Lotus

- [ ] Custom-Built Libraries
- [x] Lexer Implementation
- [x] Header Extension Support
- [x] Visual Studio Code Extension
- [ ] Parser Development with Bison (underway)
- [ ] AST - Abstract Syntax Tree (underway)
- [x] Debug Mode Configuration Settings
- [ ] LLVM Integration (in pipeline)
- [ ] Support for Writing in Different Languages within Lotus (upcoming)

## Deep Dive into Lotus

To learn more about Lotus, our comprehensive documentation is the perfect starting point. You can access it on our GitHub page: [Lotus Documentation](https://github.com/BreakerLabs/docs).

## Lotus for Visual Studio Code

To enhance the experience of writing and debugging Lotus code, we've developed a Visual Studio Code extension. You can access it here: [Lotus VSCode Extension](https://github.com/BreakerLabs/LLS-vscode).

## Project Structure

| Directory                | Contents                                                                                              |
|--------------------------|-------------------------------------------------------------------------------------------------------|
| `.github/workflow`       | here is the files that provides helps us test the code and more.                                      |
| `.github/ISSUE_TEMPLATE` | here is the files that are being used as templates when creating an issue on our github repository.   |
| `bin/`                   | here is the compiled binary files generated when compiling.                                           |
| `config/`                | here is the files to easily change modes etc.                                                         |
| `grammar/`               | here is the grammar that is specified for the programming language.                                   |
| `logs/`                  | all of your logs when you execute a Lotus program will be added here.                                 |
| `sdl/`                   | the lotus standard library files will be located here.                                                |
| `src/`                   | here you will find the core of the programming language and this includes the lexer, parser and more. |
| `tests/`                 | here you will find the tests that is made by the development team to test the internal logic.         |

## How to build the Project

First off make sure that you have all prerequisites installed on your machine and that your machine supports x86_64 assembly (AMD_64). This can be done in a terminal or command line interface.

## How to run the project

To run a Lotus project, you need to use the `lotus` command followed by the filename and (_optional: a file destination_). This can be done in a terminal or command line interface. Here is the basic command structure:

```shell
# To run a Lotus program, use the `Lotus` command followed by the filename
# Replace `filename` with the path to your .lts or .lotus file
lotus myProgram.lts myExecutable 
```