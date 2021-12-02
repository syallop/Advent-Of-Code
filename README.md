# Advent Of Code

This repository contains an attempt at completing 2020 & 2021's [Advent Of Code](https://adventofcode.com) in Haskell.

The self-imposed, time-wasting constraints for additional 'fun' are:
- Every solution must semi-gracefully deal with errors (malformed input, violated constraints etc) with an understandable error message.
  - This means no exceptions, partial functions or making assumptions about the input.
- The only 'external' libraries permitted are things in base/ prelude and a couple of standard containers like `Map`, `Set` & `Text`. Anything else can be defined in shared utils.
- Prefer to model the domain with not-too-strong types and write composable functions.
  - Optimise for readability rather than writing the program itself quickly. We're not going to top any leaderboards but we might want to reuse concepts in later days/ years and an inlined mess isn't helpful for that.

Shared utils currently define:
- A few Applicative `Parser` combinator functions.
- An `Advent Of Code` monadic type that handles failure or success
- A semi-structured `Error` type that holds an error message and key-value map of attributes.
  - Although this is a toy codebase this seems to be a good weight-power ratio to debugging code without exceptions or uninformative/ mangled println debugging.

# Notes for next year...
## Build

```
stack build
```

## Run

```
stack exec Day*
```

## Develop

- Create a `2020/D/` directory with a:
  - `*.cabal`
  - `*.hs` with a main.
- Remember to add to the root `stack.yaml`

- Import the root `AOC` module
- Define a `Solution` with:
  - A parser that transforms some input (likely text lines) to a domain type
  - Two parts that accept the input, and produce some result.

E.G.
```
solution :: Solution Int Int
solution targetTotal = Solution
  { _parse   = mapM parseDecimal
  , _partOne = \expenses -> fmap (uncurry (*))             $ oneExpensePairSumsTo   expenses targetTotal
  , _partTwo = \expenses -> fmap (\(x,y,z) -> (x * y * z)) $ oneExpenseTripleSumsTo expenses targetTotal
  }
```

Define a main like:
```
main :: IO ()
main = do
  expenses <- readLinesFromFile "2020/1/expenses"
  execSolution expenses (solution targetTotal)
```
To read text lines from a file, parse it into the domain type and then run and print the result of both parts.

All functions operate in `AOCM` to allow semi-structured error messages. The `Parser`s can be easily ran in `AOCM`, either for the initial parse step or as part of the parts.

