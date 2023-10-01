module LispErrors where

import Control.Monad.Except
import Lisp
import Text.ParserCombinators.Parsec (ParseError)

type ThrowsError = Either LispError

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

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
