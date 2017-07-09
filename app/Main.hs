module Main where

import Lib

data BrainFuckCommand = GoLeft
                      | GoRight
                      | Increment
                      | Decrement
                      | Print
                      | Read
                      | LoopL
                      | LoopR
                      | Comment Char

data BrainFuckSource = BrainFuckSource [BrainFuckCommand]
instance Show BrainFuckSource where
  show (BrainFuckSource commands) = map bfToChar commands
    where
      bfToChar x = case x of
        GoRight   -> '>'
        GoLeft    -> '<'
        Increment -> '+'
        Decrement -> '-'
        Print     -> '.'
        Read      -> ','
        LoopL     -> '['
        LoopR     -> ']'
        Comment c -> c

parseBrainFuck :: String -> BrainFuckSource
parseBrainFuck = BrainFuckSource . map charToBF
  where
    charToBF x = case x of
      '>' -> GoRight
      '<' -> GoLeft
      '+' -> Increment
      '-' -> Decrement
      '.' -> Print
      ',' -> Read
      '[' -> LoopL
      ']' -> LoopR
      c   -> Comment c

main :: IO ()
main = putStrLn "Hello world"
