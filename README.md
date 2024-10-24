# `hat`

Auto-generated AT protocol types in Haskell

## Overview

- `hat-types`

Auto-generated types

- `hat-lexicon`

`lexicon` file format implementation with parsers and code generators.

- `hat-types-gen`

Vendored `lexicon` files and tiny wrapper over generators from `hat-lexicon`

## Updating bindings

After changing lexicon files in `./hat-types-gen/lexicons` you can regenerate Haskell files by running. It will re-run code generator and pre-commit hooks will format them and adjust the list of modules in the cabal file.
```
cabal run hat-types-gen && pre-commit run -a
```
