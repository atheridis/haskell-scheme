module Main (main) where

import Data.Bifunctor
import Data.Complex
import Data.Ratio
import Numeric (readBin, readFloat, readHex, readOct)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispNum
  = LInt Integer
  | LRatio Rational
  | LFloat Double
  | LComplex (Complex Double)

instance Enum LispNum where
  toEnum = LInt . fromIntegral
  fromEnum (LInt n) = fromIntegral n

instance Eq LispNum where
  (==) (LInt n) (LInt m) = n == m
  (==) (LRatio n) (LRatio m) = n == m
  (==) (LFloat n) (LFloat m) = n == m
  (==) (LComplex n) (LComplex m) = n == m
  (==) (LInt n) (LRatio m) = fromInteger n == m
  (==) (LInt n) (LFloat m) = fromInteger n == m
  (==) (LInt n) (LComplex m) = fromInteger n == m
  (==) (LRatio n) (LFloat m) = fromRational n == m
  (==) (LRatio n) (LComplex m) = fromRational n == m
  (==) (LFloat n) (LComplex m) = (n :+ 0) == m
  a == b = b == a

instance Ord LispNum where
  (<=) (LInt n) (LInt m) = n <= m
  (<=) (LRatio n) (LRatio m) = n <= m
  (<=) (LFloat n) (LFloat m) = n <= m
  (<=) (LComplex n) (LComplex m) = (realPart . abs $ n) <= (realPart . abs $ m)
  (<=) (LInt n) (LRatio m) = fromInteger n <= m
  (<=) (LInt n) (LFloat m) = fromInteger n <= m
  (<=) (LInt n) (LComplex m) = fromInteger n <= (realPart . abs $ m)
  (<=) (LRatio n) (LFloat m) = fromRational n <= m
  (<=) (LRatio n) (LComplex m) = fromRational n <= (realPart . abs $ m)
  (<=) (LFloat n) (LComplex m) = n <= (realPart . abs $ m)
  a <= b = b <= a

instance Real LispNum where
  toRational (LInt n) = toRational n
  toRational (LRatio n) = n
  toRational (LFloat n) = toRational n

instance Integral LispNum where
  quotRem (LInt n) (LInt m) = bimap LInt LInt $ quotRem n m
  toInteger (LInt n) = n

instance Show LispNum where
  show (LInt n) = show n
  show (LFloat f) = show f
  show (LRatio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
  show (LComplex (a :+ b)) = show a ++ "+" ++ show b ++ "i"

instance Fractional LispNum where
  fromRational = LRatio
  recip (LInt n) = LRatio . recip . fromInteger $ n
  recip (LRatio n) = LRatio . recip $ n
  recip (LFloat n) = LFloat . recip $ n
  recip (LComplex n) = LComplex . recip $ n

instance Num LispNum where
  (+) (LInt n) (LInt m) = LInt (n + m)
  (+) (LRatio n) (LRatio m) = LRatio (n + m)
  (+) (LFloat n) (LFloat m) = LFloat (n + m)
  (+) (LComplex n) (LComplex m) = LComplex (n + m)
  (+) (LInt n) (LRatio m) = LRatio (n % 1 + m)
  (+) (LInt n) (LFloat m) = LFloat (fromIntegral n + m)
  (+) (LInt n) (LComplex m) = LComplex (fromIntegral n + m)
  (+) (LRatio n) (LFloat m) = LFloat (fromRational n + m)
  (+) (LRatio n) (LComplex m) = LComplex (fromRational n + m)
  (+) (LFloat n) (LComplex m) = LComplex ((n :+ 0) + m)
  a + b = b + a

  (*) (LInt n) (LInt m) = LInt (n * m)
  (*) (LRatio n) (LRatio m) = LRatio (n * m)
  (*) (LFloat n) (LFloat m) = LFloat (n * m)
  (*) (LComplex n) (LComplex m) = LComplex (n * m)
  (*) (LInt n) (LRatio m) = LRatio (n % 1 * m)
  (*) (LInt n) (LFloat m) = LFloat (fromIntegral n * m)
  (*) (LInt n) (LComplex m) = LComplex (fromIntegral n * m)
  (*) (LRatio n) (LFloat m) = LFloat (fromRational n * m)
  (*) (LRatio n) (LComplex m) = LComplex (fromRational n * m)
  (*) (LFloat n) (LComplex m) = LComplex ((n :+ 0) * m)
  a * b = b * a

  abs (LInt n) = LInt (abs n)
  abs (LRatio n) = LRatio (abs n)
  abs (LFloat n) = LFloat (abs n)
  abs (LComplex n) = LComplex (abs n)

  signum (LInt n) = LInt (signum n)
  signum (LRatio n) = LRatio (signum n)
  signum (LFloat n) = LFloat (signum n)
  signum (LComplex n) = LComplex (signum n)

  negate (LInt n) = LInt (negate n)
  negate (LRatio n) = LRatio (negate n)
  negate (LFloat n) = LFloat (negate n)
  negate (LComplex n) = LComplex (negate n)

  fromInteger = LInt

data LispVal
  = Atom String
  | Bool Bool
  | Character Char
  | String String
  | Number LispNum
  | List [LispVal]
  | DottedList [LispVal] LispVal

instance Show LispVal where
  show (Atom a) = a
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = "#\\" ++ [c]
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (List l) = "(" ++ (unwords . map show) l ++ ")"
  show (DottedList h t) = "(" ++ (unwords . map show) h ++ " . " ++ show t ++ ")"

toDouble :: LispVal -> Double
toDouble (Number (LInt n)) = fromIntegral n
toDouble (Number (LRatio n)) = (fromInteger . numerator $ n) / (fromInteger . denominator $ n)
toDouble (Number (LFloat n)) = n

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Number _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", lispBinOp (+)),
    ("-", lispBinOp (-)),
    ("*", lispBinOp (*)),
    ("/", lispBinOp (/)),
    ("mod", lispBinOp mod),
    ("quotient", lispBinOp quot),
    ("remainder", lispBinOp rem),
    ("symbol?", Bool . all lispIsSymbol),
    ("string?", Bool . all lispIsString),
    ("number?", Bool . all lispIsNumber),
    ("bool?", Bool . all lispIsBool),
    ("list?", Bool . all lispIsList)
  ]

lispBinOp :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> LispVal
lispBinOp op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> LispNum
unpackNum (Number n) = n
unpackNum (List [Number n]) = n
unpackNum _ = 0

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

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
