
# Static Analysis Tool for Toy Language

## Description
This static analysis tool is designed to parse a toy programming language and estimate the range of values that each variable can take. It leverages predefined abstractions to perform static analysis, making it a useful tool for understanding variable behavior in simple programs.

## Features
- Parses a toy language with basic constructs such as assignments, conditionals, and loops.
- Estimates variable ranges using static analysis techniques.
- Pretty prints the parsed program for better readability.
- Written in Haskell, showcasing functional programming principles.

## Installation
To install the tool, ensure you have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed. Then, clone the repository and build the project:

```bash
git clone <repository-url>
cd <repository-directory>
cabal build
```

## Usage
After building the project, you can run the tool with a toy program as input. For example:

```bash
cabal run -- <path-to-your-toy-program>
```

Or the program can be entered directly into the terminal after running the tool with no arguments:

```bash
cabal run
```

The tool will parse the program and output the estimated ranges for each variable.

