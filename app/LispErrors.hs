module LispErrors
  ( LispError (..),
    IOThrowsError,
    ThrowsError,
    liftThrows,
    extractValue,
    trapError,
    runIOThrows,
  )
where

import Control.Monad.Except
import Lisp
import Text.ParserCombinators.Parsec (ParseError)

type ThrowsError = Either LispError

-- IOThrowsError x = ExceptT { runExceptT :: IO (Either LispError x) }
--                 = ExceptT { runExceptT :: IO (ThrowsError x) }
type IOThrowsError = ExceptT LispError IO

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) =
    "Expected "
      ++ show expected
      ++ " args; found values "
      ++ (unwords . map show) found
  show (TypeMismatch expected found) =
    "Invalid type: expected "
      ++ expected
      ++ ", found "
      ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = pure val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)
