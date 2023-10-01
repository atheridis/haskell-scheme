module LispParse where

import Control.Monad.Except
import Data.Complex (Complex (..))
import Data.Ratio (denominator, numerator, (%))
import Lisp
import LispErrors
import LispNum
import Numeric (readBin, readFloat, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> pure val

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf ['\\', '"'] <|> escapedChars)
  char '"'
  pure $ String x
  where
    escapedChars :: Parser Char
    escapedChars = do
      char '\\'
      x <- anyChar
      pure $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  pure . Atom $ first : rest

parseNumber :: Parser LispVal
parseNumber = Number . LInt . read <$> (parseHex <|> parseOct <|> parseBin <|> parseDec)
  where
    parseOct :: Parser [Char]
    parseOct =
      try $
        string "#o"
          >> (show . fst . head . readOct <$> many1 octDigit)
    parseHex :: Parser [Char]
    parseHex =
      try $
        string "#x"
          >> (show . fst . head . readHex <$> many1 hexDigit)
    parseBin :: Parser [Char]
    parseBin =
      try $
        string "#b"
          >> (show . fst . head . readBin <$> many1 (oneOf "01"))
    parseDec :: Parser [Char]
    parseDec = try (string "#d" >> many1 digit) <|> many1 digit

parseBool :: Parser LispVal
parseBool = do
  try $ char '#'
  x <- oneOf "tf"
  pure $ case x of
    't' -> Bool True
    'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  Character <$> anyChar

parseFloat :: Parser LispVal
parseFloat = do
  x1 <- many1 digit
  char '.'
  x2 <- many1 digit
  pure . Number . LFloat . fst . head . readFloat $ x1 ++ "." ++ x2

parseRatio :: Parser LispVal
parseRatio = do
  x1 <- many1 digit
  char '/'
  x2 <- many1 digit
  pure . Number . LRatio $ read x1 % read x2

parseComplex :: Parser LispVal
parseComplex = do
  real <- try parseFloat <|> parseNumber
  char '+'
  im <- try parseFloat <|> parseNumber
  char 'i'
  pure . Number . LComplex $ toDouble real :+ toDouble im
  where
    toDouble :: LispVal -> Double
    toDouble (Number (LInt n)) = fromIntegral n
    toDouble (Number (LRatio n)) = (fromInteger . numerator $ n) / (fromInteger . denominator $ n)
    toDouble (Number (LFloat n)) = n

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  pure $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  pure $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  try parseAtom
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      pure x

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = pure val
eval val@(Bool _) = pure val
eval val@(Character _) = pure val
eval val@(Number _) = pure val
eval (List [Atom "quote", val]) = pure val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognised primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", lispBinOp (+)),
    ("-", lispBinOp (-)),
    ("*", lispBinOp (*)),
    ("/", lispBinOp (/)),
    ("mod", lispBinOp mod),
    ("quotient", lispBinOp quot),
    ("remainder", lispBinOp rem),
    ("symbol?", Right . Bool . all lispIsSymbol),
    ("string?", Right . Bool . all lispIsString),
    ("number?", Right . Bool . all lispIsNumber),
    ("bool?", Right . Bool . all lispIsBool),
    ("list?", Right . Bool . all lispIsList)
  ]

lispBinOp :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
lispBinOp op [] = throwError $ NumArgs 2 []
lispBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
lispBinOp op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = pure n
unpackNum (List [Number n]) = pure n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

lispIsSymbol, lispIsString, lispIsNumber, lispIsBool, lispIsList :: LispVal -> Bool
lispIsSymbol (Atom _) = True
lispIsSymbol _ = False
lispIsString (String _) = True
lispIsString _ = False
lispIsNumber (Number _) = True
lispIsNumber _ = False
lispIsBool (Bool _) = True
lispIsBool _ = False
lispIsList (List _) = True
lispIsList _ = False
