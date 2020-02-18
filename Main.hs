-- | Main module that ties together the Parsing, Expr and REPL modules
module Main where

import Parsing
import Expr
import REPL

-- | Main function that when called begins the 'repl' loop with an empty 'initState' object
main :: IO ()
main = repl initState

