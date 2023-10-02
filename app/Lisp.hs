module Lisp where

import LispNum

data LispVal
  = Atom String
  | Bool Bool
  | Character Char
  | String String
  | Number LispNum
  | List [LispVal]
  | DottedList [LispVal] LispVal
  deriving (Eq)

instance Show LispVal where
  show (Atom a) = a
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = "#\\" ++ [c]
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (List l) = "(" ++ (unwords . map show) l ++ ")"
  show (DottedList h t) = "(" ++ (unwords . map show) h ++ " . " ++ show t ++ ")"

