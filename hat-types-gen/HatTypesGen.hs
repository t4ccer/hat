module Main where

import Hat.Lexicon.Printer.Haskell (generateLexicon)

main :: IO ()
main = do
  generateLexicon "./hat-types-gen/lexicons" "./hat-types"
