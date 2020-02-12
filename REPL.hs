module REPL where

import Expr
import Parsing

data State = State { vars :: [(Name, Int)],
                     history :: [Command] }

initState :: State
initState = State [] []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name int [] = [(name, int)]
updateVars name int ((a, b):vs) = if a == name
                                  then do dropVar name ((a, b):vs)
                                          updateVars name int vs
                                  else updateVars name int vs

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name [] = []
dropVar name ((a, b):vs) | name == a   = dropVar name vs
                         | otherwise   = (a, b) : dropVar name vs

-- Add a command to the command history in the state
addHistory :: State -> Command -> State
addHistory st cmd = st { history = cmd : history st }

process :: State -> Command -> IO ()
process st (Set var e)
     = do let st' = addHistory st (Set var e)
          -- st' should include the variable set to the result of evaluating e
          repl st'
process st (Eval e)
     = do --let st' = addHistory st (Eval e)
         -- let Just n = eval (vars st) e

          case eval (vars st) e of
               Just n -> do let [newVars] = updateVars "it" n (vars st)
                            let st' = addHistory st (Eval e)
                            let st'' = State [(newVars)] (history st')
                            putStrLn (show n)
                            --putStrLn (show (vars st'')) --Commented out (for illustration of working variable storage)
                            repl st''

               Nothing -> putStrLn "Invalid Number!"
          -- Print the result of evaluation
          repl st


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: State -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             --putStr (show (length (vars st)) ++ " > ")
             inp <- getLine
             if inp == "quit"
                  then do {putStrLn "bye"; return ()}
                  else case parse pCommand inp of
                    [(cmd, "")] -> -- Must parse entire input
                            process st cmd
                    _ -> do putStrLn "Parse error"
                            repl st


