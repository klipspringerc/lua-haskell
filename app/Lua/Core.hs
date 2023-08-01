module Lua.Core where

import Prelude hiding (lookup)
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Data.Typeable
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import GHC.Generics (Generic)
import Data.Hashable 


--- ### Environment
type Env = H.HashMap String Val

--- ### Values
-- supported basic types in Lua: nil, boolean, number, string, table, function.
-- control value is introduced to pass execution flow control signals.
-- omitted: userdata, thread

-- Table
-- key can contain values of all types (except nil). 
-- Any key associated to the value nil is not considered 
-- part of the table. Conversely, any key that is not part of a 
-- table has an associated value nil.
type Table = H.HashMap Val Val 

data Val = NilVal  -- cannot be used as table index 
     | BoolVal Bool
     | IntVal Int 
     | StrVal String   
     | TableVal Table  
     | FuncVal [String] Stmt Env  -- Closure
     | ControlVal String  -- Control signals
    deriving (Typeable, Generic, Eq)

instance Hashable Val

typeName :: Val -> String
typeName NilVal = "Nil"
typeName BoolVal{} = "Boolean"
typeName IntVal{} = "Integer"
typeName StrVal{} = "String"
typeName TableVal{} = "Table"
typeName FuncVal{} = "Func"
typeName ControlVal{} = "Control"

instance Show Val where
  show NilVal = "nil"
  show (TableVal table) = "{" ++ (show table) ++ "}"  
  show (BoolVal b) = if b == True then "true" else "false"
  show (IntVal i) = show i
  show (StrVal s) =  s
  show (FuncVal args _ _)  = "#<function:(" ++ unwords args ++ ") ...>"
  show (ControlVal s) =  "#<control:(" ++ s ++ ")>"

--- ### Expressions
-- program expressions that can be evaluated to Val

data Exp = NilExp
     | IntExp Int
     | BoolExp Bool
     | StrExp String -- literal string, simply evaluate to StrVal 
     | VarExp String  -- before first assignment to a variable, value is nil 
     | UnopExp String Exp    
     | BinopExp String Exp Exp 
     | TableConstructor [(Exp,Exp)]  -- evaluate to a TableVal     
     | TableLookUpExp Exp Exp -- take a VarExp that evaluate to a TableVal and key expression
     | FuncCallExp Exp [Exp] -- take a function name and argument list
     | MethodCallExp Exp String [Exp] -- take a table, method name and argument list
  deriving (Eq, Generic, Show)

instance Hashable Exp

--- ### Statements

data Stmt = AssignStmt [Exp] [Exp] -- variable assignment, support multiple assignment
          | TableAssignStmt Exp Exp Exp -- t[key] = val 
          | PrintStmt Exp -- printing
          | SeqStmt [Stmt] -- a sequence of statements to be executed 
          | ReturnStmt Exp
          | FuncStmt String [Exp] Stmt
          | MethodStmt String Stmt -- table name and a FuncStmt
          | IfStmt Exp Stmt Stmt -- else/elseif would be recursively stored in the second Stmt
          | ForStmt String Exp Exp Exp Stmt -- var name, start, end, step and body
          | WhileStmt Exp Stmt -- condition expression and body
          | BreakStmt
          | NullStmt
          | QuitStmt
    deriving (Eq, Generic, Show)


instance Hashable Stmt

 --- ### Diagnostic

--- In our monadic evaluator, all you need to throw an diagnostic message is to
--- call `throwError`, e.g. `throwError $ NotASymbol v`. Pick the right diagnosic
--- to throw!
data Diagnostic = UnexpectedArgs [Val]
                | TypeError Val
                | NotFuncError Val
                | UndefSymbolError String
                | NotArgumentList Val
                | InvalidSpecialForm String Val
                | CannotApply Val [Val]
                | InvalidExpression Exp
                | NotASymbol Exp
                | NotAListOfTwo Val
                | UnquoteNotInQuasiquote Val
                | Unimplemented String

err_type :: Diagnostic -> String
err_type (UndefSymbolError _) = "ERROR: undef_symbol"
err_type (NotASymbol _) = "ERROR: nota_symbol"
err_type (InvalidSpecialForm _ _) = "ERROR: invalid_special_form"
err_type (UnquoteNotInQuasiquote _) = "ERROR: unquote_notin_quasiquote"
err_type (UnexpectedArgs _) = "ERROR: unexpected_args"
err_type (TypeError _) = "ERROR: type_error"
err_type _ = "ERROR: other"

instance Show Diagnostic where
  show (UnexpectedArgs actual) =
    "Error: Unexpected arguments or wrong number of arguments (" ++ unwords (map show actual) ++ ")"
  show (TypeError v) =
    "Error: Value " ++ show v ++ " has unexpected type " ++ typeName v
  show (NotFuncError val) =
    "Error: " ++ show val ++ " is not a function"
  show (UndefSymbolError name) =
    "Error: Symbol " ++ name ++ " is undefined"
  show (NotArgumentList val) =
    "Error: Expecting an argument list, but found " ++ show val
  show (InvalidSpecialForm f val) =
    "Error: Invalid pattern in special form `" ++ f ++ "`: " ++ show val
  show (CannotApply val args) =
    "Error: Cannot apply " ++ show val ++ " on argument list (" ++ unwords (map show args) ++ ")"
  show (InvalidExpression val) =
    "Error: Invalid expression: " ++ show val
  show (NotASymbol val) =
    "Error: Not a symbol: " ++ show val
  show (NotAListOfTwo val) =
    "Error: Not a list of two elements: " ++ show val
  show (UnquoteNotInQuasiquote val) =
    "Error: `unquote` not in a `quasiquote` context: " ++ show val
  show (Unimplemented feature) =
    "Error: " ++ feature ++ " is not implemented. You should implement it first!"


type EvalState a = StateT Env (Except Diagnostic) a
