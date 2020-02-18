-- | Handles overarching 'repl' loop of the program and 'State' operations such as updating History or removing variables.
module REPL where

import Expr
import Parsing
import Data.Typeable
import Data.Char (isDigit)

import Control.Exception
import System.IO.Error


-- | State is a data type that stores the history of commands in the program and the current value of any variables
data State = State { vars :: [(Name, Int) ],  -- ^ Variable History
                     history :: [Command] -- ^ History Tracking
                     } -- ^ Generic State Object

-- | Creation of empty State object
initState :: State
initState = State [] []

-- | Takes in the name of a variable and the new value of the variable, plus a list of the current variables. Will remove any variables that currently exist within the list via 'dropVar' and then return the new list of variables
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name int [] = [(name, int)]
updateVars name int (v:vs) = do let newVars = dropVar name (v:vs) --  Calls dropVar if the variable already exists
                                newVars ++ [(name, int)]


-- | Return a new set of variables with the given name removed using the current list passed in by 'updateVars'
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name ((a,b):vs) = filter (\(a,_) -> a /= name) ((a,b):vs) --Uses filter to loop through each position in the list


-- | Add a command to the command history in the 'State'
addHistory :: State -> Command -> State
addHistory st cmd = do let st' = State (vars st) (history st ++ [cmd])
                       st'

-- | Checks the length of a list (-1) is more than the index trying to be accessed and returns a Bool result
checkLength :: Int -> [Command] -> Bool
checkLength request history = do let lengthOf = (length history) -1
                                 if lengthOf < request
                                     then False
                                     else True


-- | Checks that the point being accessed in the state's history is valid and if so passes the command from that point into the 'process' function
historyCheck :: State -> [Char] -> IO ()
historyCheck st cmd = do if (length cmd) == 1 --Stops the operation if the only entry is "!"
                         then do putStrLn "Parse error"
                                 repl st
                         else if checkDigits (drop 1 cmd) == True -- Checks that everything after the "!" is a digit to stop read function crash
                                then do let request = drop 1 cmd
                                        let requestInt = read request :: Int --Casts the list of chars to a single integer
                                        if (checkLength requestInt (history st) == True) --Checks that point exists in the state's history
                                             then process st (history st !!(requestInt))
                                             else do putStrLn "Invalid history point"
                                                     repl st
                         else do putStrLn "History cannot contain non-integers"
                                 repl st

-- | Recursively loops through the list of Chars to check if they are all digits or not, and then returns a Bool value
checkDigits :: [Char] -> Bool
checkDigits [] = True -- Empty string result must be true or result of function will always be false
checkDigits (x:xs) = do if isDigit x == True then checkDigits xs
                        else False


--fileSt :: State
--fileSt = State [] []

-- | Another version of 'process' that is used for file reading as this one loops back into the 'processFile' method rather than going on to the 'repl' loop, allowing the program to read files with multiple lines
processLine :: State -> Command -> [String] -> IO ()
processLine st (Set var e) cs -- Case for variable operations
     = do case eval (vars st) e of
               Just n -> do let newVars = updateVars var n (vars st) -- Case if a variable was succesfully found
                            let st' = addHistory st (Set var e)
                            let st'' = State (newVars) (history st')
                            putStrLn "OK"
                            processFile st'' cs

               Nothing -> do let initState = st--Case for if a variable has not been declared before being accessed
                             putStrLn "Variable has not been declared!"
                             processFile st cs


processLine st (Eval e) cs --Case for Expressions
     = do --let st' = addHistory st (Eval e)
         -- let Just n = eval (vars st) e
          case eval (vars st) e of
               Just n -> do let newVars = updateVars "it" n (vars st) -- Case if an expression was valid
                            let st' = addHistory st (Eval e)
                            let st'' = State (newVars) (history st')
                            putStrLn (show n)
                            processFile st'' cs

               Nothing -> do putStrLn "Variable has not been declared!" -- Case if an expression was invalid (error message mentions variables as only error that could reach her would be an incorrect variable expression e.g. a + 1 when a doesn't exist)
                             processFile st cs

          -- Print the result of evaluation
processLine st (Quit) cs -- Case for a quit command
     = do putStrLn "bye"

processLine st (Read f) cs -- Case for a file read command
     = do content <- readFile f
          let linesInFile = lines content
          processFile st linesInFile


-- | Special version of 'repl' that is used to parse the operations of a read in file that will not loop back on itself if there is an error
parseOpr :: State -> String -> [String] -> IO ()
--parseOpr st ""  = repl st
parseOpr st opr cs =
                        case parse pCommand opr of
                              [(cmd, "")] -> processLine st cmd cs
                              _ -> do putStrLn "Parse error"


-- | Function that is used to process the list of commands read in from a file
processFile :: State -> [String] -> IO ()
processFile st [] = repl st
processFile st (c:cs) =
                           parseOpr st c cs


-- | Process function that has multiple different results depending on the type of command that has been entered that checks if the command is valid or not according to the 'eval' function and then performs the correct operation depending on the results of 'eval'
process :: State -> Command -> IO ()
process st (Set var e) -- Case for variables operations
     = do --putStrLn (show var)
          --putStrLn(show (vars st))
          --putStrLn (show e)
          case eval (vars st) e of
               Just n -> do let newVars = updateVars var n (vars st) -- Case for if retrieving the value of the variable was successful
                            let st' = addHistory st (Set var e)
                            let st'' = State (newVars) (history st')
                            putStrLn "OK"
                            repl st''

               Nothing -> do putStrLn "Variable has not been declared!" -- Case for if the value of the variable couldn't be accessed (i.e. it doesn't exist)
                             repl st


process st (Eval e) -- Case for expressions
     = do --let st' = addHistory st (Eval e)
         -- let Just n = eval (vars st) e
          case eval (vars st) e of
               Just n -> do let newVars = updateVars "it" n (vars st) --Case for if the expression is correct and successful
                            let st' = addHistory st (Eval e)
                            let st'' = State (newVars) (history st')
                            putStrLn (show n)
                            repl st''

               Nothing -> do putStrLn "Variable has not been declared or division by 0 attempted!" -- Case if an expression was invalid (error message mentions variables as only error that could reach her would be an incorrect variable expression e.g. a + 1 when a doesn't exist)
                             repl st

          -- Print the result of evaluation
process st (Quit) -- Case for quit command
     = do putStrLn "bye"
          return ()



process st (Read f) -- Case for file read command
          = do content <- tryIOError $ readFile f
               case content of
                         Left except -> do putStrLn ("File not found — " ++ (show except))
                                           repl st
                         Right contents -> processFile st (lines contents)
-- | Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.
repl :: State -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             inp <- getLine
             if length inp > 0 then
               if (head inp) == '!' then do {historyCheck st inp}
               else case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                         process st cmd
                    _ -> do putStrLn "Parse error" -- Returns error if parser can't process error
                            repl st
             else do putStrLn "Cannot have empty input"
                     repl st


