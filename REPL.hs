module REPL where

import Expr
import Parsing
import Data.Typeable
import Data.Char (isDigit)



data State = State { vars :: [(Name, Int)],
                     history :: [Command] }

initState :: State
initState = State [] []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name int [] = [(name, int)]
updateVars name int (v:vs) = do let newVars = dropVar name (v:vs)
                                newVars ++ [(name, int)]
                                     

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name ((a,b):vs) = filter (\(a,_) -> a /= name) ((a,b):vs)


-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = do let st' = State (vars st) (history st ++ [cmd])
                       st'
                              --else let st' = State (vars st) (history st ++ [cmd])

checkLength :: Int -> [Command] -> Bool
checkLength request history = do let lengthOf = (length history) -1
                                 if lengthOf < request
                                     then False
                                     else True


historyCheck :: State -> [Char] -> IO ()
historyCheck st cmd = do if (length cmd) == 1
                         then do putStrLn "Parse error"
                                 repl st
                         else if checkDigits (drop 1 cmd) == True 
                                then do let request = drop 1 cmd
                                        let requestInt = read request :: Int
                                        if (checkLength requestInt (history st) == True)
                                             then process st (history st !!(requestInt))
                                             else do putStrLn "Invalid history point"
                                                     repl st
                         else do putStrLn "History cannot contain non-integers"
                                 repl st  

checkDigits :: [Char] -> Bool
checkDigits [] = True
checkDigits (x:xs) = do if isDigit x == True then checkDigits xs
                        else False




process :: State -> Command -> IO ()
process st (Set var e)
     = do --putStrLn (show var)
          --putStrLn(show (vars st))
          --putStrLn (show e)
          case eval (vars st) e of
               Just n -> do let newVars = updateVars var n (vars st)
                            let st' = addHistory st (Set var e)
                            let st'' = State (newVars) (history st')
                            putStrLn "OK"
                            repl st''
               
               Nothing -> do putStrLn "Variable has not been declared!"
                             repl st
         

          -- st' should include the variable set to the result of evaluating e
process st (Eval e)
     = do --let st' = addHistory st (Eval e)
         -- let Just n = eval (vars st) e
          putStrLn( show (vars st))

          case eval (vars st) e of
               Just n -> do let newVars = updateVars "it" n (vars st)
                            let st' = addHistory st (Eval e)
                            let st'' = State (newVars) (history st')
                            putStrLn (show n)
                            --print (show (history st'')) --Commented out (for illustration of working variable storage)
                            repl st''

               Nothing -> do putStrLn "Variable has not been declared!"
                             repl st

          -- Print the result of evaluation
process st (Quit)
     = do putStrLn "bye"
          return ()



process st (File f)
     = do putStrLn (show f)
          content <- readFile f
          let linesInFile = lines content
          repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             inp <- getLine
             if (head inp) == '!' then do {historyCheck st inp}
             else case parse pCommand inp of
                [(cmd, "")] -> -- Must parse entire input
                        process st cmd
                _ -> do putStrLn "Parse error"
                        repl st


