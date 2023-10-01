module Main (main) where

import LispParse (readExpr, eval)
import LispErrors
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  let s = readExpr . head $ args
  putStrLn $ extractValue $ trapError $ fmap show $ s >>= eval
