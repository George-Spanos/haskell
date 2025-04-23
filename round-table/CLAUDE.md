# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands
- Build: `stack build`
- Run: `stack run` (Minesweeper web server on port 3001)
- Test all: `stack test`
- Run specific test: `stack test --test-arguments="--match 'Minesweeper.Game/pattern'"` 
- Run ghci: `stack ghci`
- Check types: `stack build --fast`

## Code Style Guidelines
- Indentation: 2 spaces
- Extensions: Place at top of file `{-# LANGUAGE OverloadedStrings #-}`
- Imports: Group imports, use qualified imports (e.g., `as Map`), explicit imports
- Types: Always include explicit type signatures, use descriptive type aliases
- Naming: camelCase for variables/functions, PascalCase for types/constructors
- Error handling: Use Maybe for optional values, pattern matching, case expressions
- Records: For complex data structures with named fields
- Pure functions: Avoid side effects, use ' for updated variables (e.g., `board'`)
- Follow compiler warnings (-Wall, -Wcompat, etc.) from package.yaml