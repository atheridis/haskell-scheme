module LispTypes where

import Control.Monad.Except (ExceptT, catchError, liftIO, liftM, runExceptT, throwError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import LispNum
import Text.ParserCombinators.Parsec (ParseError)

-- Values
data LispVal
  = Atom String
  | Bool Bool
  | Character Char
  | String String
  | Number LispNum
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

-- \| PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
-- \| Func { params :: [String], vararg :: (Maybe String),
--          body :: [LispVal], closure :: Env }

instance Show LispVal where
  show (Atom a) = a
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Character c) = "#\\" ++ [c]
  show (String s) = "\"" ++ s ++ "\""
  show (Number n) = show n
  show (List l) = "(" ++ (unwords . map show) l ++ ")"
  show (DottedList h t) = "(" ++ (unwords . map show) h ++ " . " ++ show t ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda ("
      ++ unwords args
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ") ...)"

-- Errors
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

type ThrowsError = Either LispError

-- IOThrowsError x = ExceptT { runExceptT :: IO (Either LispError x) }
--                 = ExceptT { runExceptT :: IO (ThrowsError x) }
type IOThrowsError = ExceptT LispError IO

trapError action = catchError action (pure . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = pure val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

-- Environment
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  pure value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> pure value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      pure value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = (++ env) <$> mapM addBinding bindings
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

