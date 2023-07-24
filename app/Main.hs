module Main where
import Lua.Core
import Lua.Parse
import Lua.Eval
--import Lua.Runtime
import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as H


--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  putStr "Lua> "
  hFlush stdout
  l <- getLine                                        
  case parse stmt "stdin" l of                  
    Left err       -> do print err
                         repl env
    Right QuitStmt -> printLn "bye"                        
    Right s        -> case runExcept $ runStateT (exec s) env of
                         Left err -> print err
                         Right (output, nenv) -> do print (show output)
                                                    repl nenv
  -- repl env

main :: IO ()
main = repl H.empty
