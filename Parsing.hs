{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}
 -- | Handles the tokenization of user commands and contains the grammar for the parser
module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

-- | Type that represents the skeleton of all the parsers for transferring the users input into usable commands
newtype Parser a              =  P (String -> [(a,String)])

-- | One type of Functor Parser provided by the lecturer
instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

-- | One type of Applicative Parser provided by the lecturer
instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

-- | One type of Monad Parser provided by the lecturer
instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

-- | One type of Alternative Parser provided by the lecturer
instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

-- | One type of MonadPlus Parser provided by the lecturer
instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

-- | Failure state Token
failure                       :: Parser a
failure                       =  mzero

-- | Turns input into single items
item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

-- | Parses the users original input into usable tokens
parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

-- | Function that can differetiate between which parser to use
(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

-- | Token for returning simple characters after validating them
sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

-- | Returns (after validating) a digit for use by the parser
digit                         :: Parser Char
digit                         =  sat isDigit

-- | Returns (after validating) an upper case letter for use by the parser
lower                         :: Parser Char
lower                         =  sat isLower

-- | Returns (after validating) an lower case letter for use by the parser
upper                         :: Parser Char
upper                         =  sat isUpper

-- | Returns (after validating) a simple letter for use by the parser
letter                        :: Parser Char
letter                        =  sat isAlpha

-- | Returns (after validating) a normal number for use by the parser
alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

-- | returns a simple char as a Parsed char
char                          :: Char -> Parser Char
char x                        =  sat (== x)

-- | Works through a String (As a list of chars) in order to validate
string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

-- | Allows the parsing of expressions with more than one digit next to each other (e.g. >9 or <-9)
many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

-- | Covers the many function on the left side
many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

-- | Checks that a variable given is a valid identifier
ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

-- | Used to process Natural numbers of more than one index in size
nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

-- | Used to process Integers in both regular and naegative form
int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

-- | Used to facilitate the breaking down of a file path into individual parts in order to return it as a proper String
file                          :: Parser String
file                          = do f <- many1 letter
                                   y <- char '.'
                                   fs <- many1 letter
                                   return (f ++ [y] ++ fs)
                                 ||| ident

openb                          :: Parser Char
openb                         = do char '('
                                   return '('

closingb                       :: Parser Char
closingb                      = do char ')'
                                   return ')'



-- | Used to check for spaces and remove them before passing back to the parser
space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

-- | Used to remove spaces from either side of tokens
token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

-- | Used to tokenize identifiers
identifier                    :: Parser String
identifier                    =  token ident

-- | Used to tokenize natural numbers
natural                       :: Parser Int
natural                       =  token nat

-- | Used to tokenize integers
integer                       :: Parser Int
integer                       =  token int

-- | Used to tokenize symbols
symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

-- | Used to tokenize filenames
filename                      :: Parser String
filename                      =  token file

-- | Used to tokenize open brackets
openbracket                   :: Parser Char
openbracket                   = token openb

-- | Used to tokenize closing brackets
closebracket                  :: Parser Char
closebracket                  = token closingb
