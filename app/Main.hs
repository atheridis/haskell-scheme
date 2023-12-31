module Main (main) where

import LispTypes
import LispParse (eval, readExpr, primitiveBindings)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne . head $ args
    _ -> putStrLn "Program only takes 0 or 1 arguments"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ (liftThrows . readExpr) expr >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then pure ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
