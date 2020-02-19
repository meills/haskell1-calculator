-- | Handles the parsing of user input and finds the values of expressions given in this input
module Expr where

import Parsing
import Data.Maybe

-- | Type Name is just an alias for 'String'
type Name = String
-- | Type File is just an alias for 'Name'
type File = Name

-- | Data type Expr which holds the possible signatures for each of the calculator operations
data Expr = Add Expr Expr -- ^ Addition of two Integers/Variables
          | Val Int -- ^ Retrieving value of an entry (e.g. input is just "4")
          | Name [Char] -- ^ Retrieving value of variable
          | Minus Expr Expr -- ^ Subtraction of two Integers/Variables
          | Multiply Expr Expr -- ^ Multiplication of two Integers/Variables
          | Division Expr Expr -- ^ Division of two Integers/Variables
          | Abs Expr -- ^ Find the Absolute value of an expression
          | Mod Expr Expr -- ^ Find the Modulus of two Integers/Variables
          | Power Expr Expr -- ^ Find the Power of one Integer/Variable to another Integer/Variable

  deriving Show

-- | Data type for the possible REPL commands and the signatures it can have
data Command = Set Name Expr -- ^ Variable Assignment
             | Eval Expr -- ^ Evaluation of an expression
             | Quit -- ^ Quit Command
             | Read File -- ^ Read file Command

  deriving Show

-- | Function that recursively goes through list of variables in order to find value of a variable, or returns nothing if that variable doesn't exist
retrieveVar :: [(Name, Int)] -> Name -> Maybe Int
retrieveVar [] x = Nothing -- If end of list reached, variable doesn't exist so return Nothing
retrieveVar ((a, b) : vs) x =  if a == x
                               then Just b
                               else retrieveVar vs x

-- | eval function takes in the current list of variables, the expression to be evaluated and returns a Maybe Int (in case the expression fails). This is the framework for all the caclulations the calculator can make.
eval :: [(Name, Int)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Int -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly

eval vars (Name x) = retrieveVar vars x -- Retrive value of a variable

eval vars (Add x y) = do q <-  (eval vars x) -- Add together two values
                         p <-  (eval vars y)
                         Just (q + p) -- Just ((eval vars x) + (eval vars y)) --fmap sum $ sequence [eval vars x, eval vars y]  -- return an error (because it's not implemented yet!)

eval vars (Minus x y) =  do q <-  (eval vars x) -- Minus one value from another
                            p <-  (eval vars y)
                            Just (q - p)

eval vars (Multiply x y) = do q <-  (eval vars x) -- Multiply together two values
                              p <-  (eval vars y)
                              Just (q * p)

eval vars (Division x y) = do q <-  (eval vars x) -- Divide one value by another value
                              p <-  (eval vars y)
                              if  (p) == 0 then Nothing
                              else Just (div q p)


eval vars (Abs x) = do q <-  (eval vars x) -- Find the absolute value of a value
                       Just (abs q)

eval vars (Mod x y) = do q <-  (eval vars x) -- Find the Modulus of two values
                         p <-  (eval vars y)
                         Just (mod q p )

eval vars (Power x y) = do q <-  (eval vars x) -- Find the result of one value to the power of another value
                           p <-  (eval vars y)
                           Just (q ^ p)


-- | Function that finds what type of command the input given by the user is (Evaluation, Variable Assignment, Quit or File Read )
pCommand :: Parser Command
pCommand = do string ":q"
              return (Quit) -- Returns Quit Command
            ||| do string ":r"
                   f <- filename
                   return (Read f) -- Returns File read command with name of file to be read
                ||| do t <- identifier
                       char '='
                       e <- pExpr
                       return (Set t e) -- Returns variable assignment command with variable and value to be assigned
                    ||| do e <- pExpr
                           return (Eval e) -- Returns Evaluation to be performed

-- | Function to handle simple expressions such as 'Add' and 'Minus' that have the lowest precedence
pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e) --Returns expression for addition
            ||| do char '-'
                   e <- pExpr
                   return (Minus t e) --Returns expression for subtraction
                 ||| return t

-- | Function to handle some of the complex expressions that have higher precedence such as those containing brackets or 'Abs', or those that have to be checked first such as getting the value of variables
pFactor :: Parser Expr
pFactor = do d <- integer
             return (Val d) -- Return expression for value of a char(s) representing an integer
           ||| do v <- identifier
                  return (Name v) -- Return expression for value of a variable
                ||| do f <- file
                       return (Name f)
                     ||| do char '('
                            e <- pExpr
                            char ')'
                            return e -- Return expression for value/expression held with brackets
                          ||| do char '|'
                                 e <- pExpr
                                 char '|'
                                 return (Abs e) -- Return expression for finding absolute values

-- | Function for the normal expressions that have a medium level of precedence, including the 'Multiply', 'Division', 'Power' and 'Mod' expressions
pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Multiply f t) -- Returns muliplication expression with two values to multiply
            ||| do char '/'
                   t <- pTerm
                   return (Division f t) -- Returns division expression with value to be divided by another value
                 ||| do char '^'
                        t <- pTerm
                        return (Power f t) -- Returns power expression with a value to be put to the power of another value
                      ||| do char '%'
                             t <- pTerm
                             return (Mod f t) -- Returns modulus expression
                            ||| return f

