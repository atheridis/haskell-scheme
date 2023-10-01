module Main (main) where

import Data.Complex
import Data.Ratio
import Numeric (readBin, readFloat, readHex, readOct)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | Bool Bool
  | Character Char
  | String String
  | Number Integer
  | Float Double
  | Rational Rational
  | Complex (Complex Double)
  | List [LispVal]
  | DottedList [LispVal] LispVal

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom a) = a
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character c) = "#\\" ++ [c]
showVal (String s) = "\"" ++ s ++ "\""
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Rational r) = (show . numerator) r ++ "/" ++ (show . denominator) r
showVal (Complex (a :+ b)) = show a ++ "+" ++ show b ++ "i"
showVal (List l) = "(" ++ (unwords . map showVal) l ++ ")"
showVal (DottedList h t) = "(" ++ (unwords . map showVal) h ++ " . " ++ showVal t ++ ")"

toDouble :: LispVal -> Double
toDouble (Number n) = fromIntegral n
toDouble (Float n) = realToFrac n

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  -- Right var -> "Found value\n" ++ show var
  Right var -> "Found " ++ showVal var

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
parseNumber = Number . read <$> (parseHex <|> parseOct <|> parseBin <|> parseDec)
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
  pure . Float . fst . head . readFloat $ x1 ++ "." ++ x2

parseRatio :: Parser LispVal
parseRatio = do
  x1 <- many1 digit
  char '/'
  x2 <- many1 digit
  pure . Rational $ read x1 % read x2

parseComplex :: Parser LispVal
parseComplex = do
  real <- try parseFloat <|> parseNumber
  char '+'
  im <- try parseFloat <|> parseNumber
  char 'i'
  pure . Complex $ toDouble real :+ toDouble im

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

main :: IO ()
main = do
  (expr : d) <- getArgs
  putStrLn (readExpr expr)
