# Exploration of Lambda calculus

## Goal

Small lambda calculus based language implementation. Easy to tinker with. Show how all programs reduce to functions.

## Features

- Lambda calculus abstract syntax
- Evaluator preserves laziness of host language
- Parser with syntax sugar
  - Comments
  - Parenthesized expressions
  - Multi argument function application and abstraction:
    `x: y: z: z x y`
  - Recursive let bindings:
    `x = v; e` → `(x: e) (Y (x: v))`
  - Natural numbers:
    `3` → `Succ (Succ (Succ Zero))`
  - Lists:
    `[1, 2, 3]` → `Cons 1 (Cons 2 (Cons 3 Nil))`
- Pretty printer: `Bool`, `Nat`, `List`
- Standard library

### [Standard Library](./prelude.hal)

- Functions, fixed point combinators
- Booleans
- Natural numbers via Church encoding
- Lists

### [Example program](example.hal)

Fibonacci, factorial, numbers, lists, booleans and standard library functions combined.


### Out of scope

- Operators
- Type system

## Run

Requires: cabal-install, ghc

```sh
cabal update
cabal run halan -- example.hal
```

### Test

```sh
./test.sh
```
