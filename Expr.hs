module Expr where

import Parsing
import Data.Maybe

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to
-- add other operations, and variables
data Expr = Add Expr Expr
          | Val Int
          | Name [Char]
          | Minus Expr Expr
          | Multiply Expr Expr
          | Division Expr Expr
  deriving Show

-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
  deriving Show


retrieveVar :: [(Name, Int)] -> Name -> Maybe Int 
retrieveVar [] x = Nothing
retrieveVar ((a, b) : vs) x =  if a == x
                               then Just b
                               else retrieveVar vs x
 
eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Name x) = retrieveVar vars x
                        --Just x
eval vars (Add x y) = do q <-  (eval vars x)
                         p <-  (eval vars y)
                         Just (q + p) -- Just ((eval vars x) + (eval vars y)) --fmap sum $ sequence [eval vars x, eval vars y]  -- return an error (because it's not implemented yet!)
eval vars (Minus x y) =  do q <-  (eval vars x) 
                            p <-  (eval vars y)
                            Just (q - p)
eval vars (Multiply x y) = do q <-  (eval vars x)
                              p <-  (eval vars y)
                              Just (q * p)
eval vars (Division x y) = do q <-  (eval vars x)
                              p <-  (eval vars y)
                              Just (div q p)

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pCommand :: Parser Command
pCommand = do t <- identifier
              char '='
              e <- pExpr
              return (Set t e)
            ||| do e <- pExpr
                   return (Eval e)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Minus t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- integer
             return (Val d)
           ||| do v <- letter
                  return (Name [v])
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Multiply f t)
            ||| do char '/'
                   t <- pTerm
                   return (Division f t)
                 ||| return f

