module AST(LispVal(..)
  , showVal
  , LispError(..)
  , ThrowsError
  , IOThrowsError
  , liftThrows
  , unwordsList
  , extractValue
  , trapError
  , Env
  , nullEnv
) where

import Control.Monad.Except
import Data.IORef
import Text.ParserCombinators.Parsec(ParseError)
import System.IO(Handle)

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              | PrimimitiveFunc ([LispVal] -> ThrowsError LispVal)
              | Func { params :: [String], vararg :: (Maybe String),
                       body :: [LispVal], closure :: Env }
              | IOFunc ([LispVal] -> IOThrowsError LispVal)
              | Port Handle

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Number num) = show num
showVal (String string) = "\"" ++ string ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (PrimimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                        ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError)           = "Parse error at " ++ show parseError

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right value) = value

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

