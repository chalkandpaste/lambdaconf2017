module Main where

import Repl

main :: IO ()
main = runInputT defaultSettings repl
