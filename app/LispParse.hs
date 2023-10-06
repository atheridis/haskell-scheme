module LispParse where

import Control.Monad.Except
import Data.Complex (Complex (..))
import Data.Ratio (denominator, numerator, (%))
import Lisp
import LispErrors
import LispNum
import LispState (Env, defineVar, getVar, setVar)
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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = pure val
eval env val@(Bool _) = pure val
eval env val@(Character _) = pure val
eval env val@(Number _) = pure val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = pure val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    _ -> throwError $ TypeMismatch "boolean" pred
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognised primitive function args" func)
    ($ args)
    (lookup func primitives)

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = pure x
car [DottedList (x : _) _] = pure x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = pure $ List xs
cdr [DottedList [] y] = pure y
cdr [DottedList (_ : xs) y] = pure $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
-- cons [x1, List []] = pure $ List [x1]
cons [x, List xs] = pure $ List $ x : xs
cons [x, DottedList xs y] = pure $ DottedList (x : xs) y
cons [x1, x2] = pure $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = pure . Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = pure . Bool $ arg1 == arg2
eqv [String arg1, String arg2] = pure . Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = pure . Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = pure . Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = pure $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", lispOp (+)),
    ("-", lispOp (-)),
    ("*", lispOp (*)),
    ("/", lispOp (/)),
    ("mod", lispOp mod),
    ("quotient", lispOp quot),
    ("remainder", lispOp rem),
    ("symbol?", Right . Bool . all lispIsSymbol),
    ("string?", Right . Bool . all lispIsString),
    ("number?", Right . Bool . all lispIsNumber),
    ("bool?", Right . Bool . all lispIsBool),
    ("list?", Right . Bool . all lispIsList),
    ("=", lispBinOp unpackNum (==)),
    ("<", lispBinOp unpackNum (<)),
    (">", lispBinOp unpackNum (>)),
    ("<=", lispBinOp unpackNum (<=)),
    (">=", lispBinOp unpackNum (>=)),
    ("/=", lispBinOp unpackNum (/=)),
    ("||", lispBinOp unpackBool (||)),
    ("&&", lispBinOp unpackBool (&&)),
    ("string=?", lispBinOp unpackStr (==)),
    ("string<?", lispBinOp unpackStr (<)),
    ("string>?", lispBinOp unpackStr (>)),
    ("string<=?", lispBinOp unpackStr (<=)),
    ("string>=?", lispBinOp unpackStr (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv)
  ]

lispOp :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
lispOp op [] = throwError $ NumArgs 2 []
lispOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
lispOp op params = Number . foldl1 op <$> mapM unpackNum params

lispBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
lispBinOp unpacker op params
  | length params /= 2 = throwError $ NumArgs 2 params
  | otherwise = Bool <$> liftM2 op (unpacker . head $ params) (unpacker . last $ params)

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number n) = pure n
unpackNum (List [Number n]) = pure n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = pure b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = pure s
unpackStr notString = throwError $ TypeMismatch "string" notString

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
