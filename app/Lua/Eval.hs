
module Lua.Eval where

import Lua.Core
import Lua.Runtime

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union, delete)
import Control.Monad.State
import Control.Monad.Except

getName :: Exp -> EvalState String
getName (VarExp v) = return v
getName v = throwError $ NotASymbol v

evalPair :: (Exp, Exp) -> EvalState (Val,Val)
evalPair (e1, e2) = do v1 <- eval e1
                       v2 <- eval e2 
                       return (v1, v2)

-- ### The monadic evaluator
eval :: Exp -> EvalState Val
-- constant expressons 
eval NilExp = return NilVal
eval (IntExp i) = return $ IntVal i
eval (BoolExp b) = return $ BoolVal b
eval (StrExp s) = return $ StrVal s

-- variable expression
eval (VarExp k) = do env <- get
                     case H.lookup k env of
                        Just v  -> return v
                        Nothing -> return NilVal

-- binary operations
eval (BinopExp op e1 e2) = do v1 <- eval e1
                              v2 <- eval e2
                              case v1 of
                                TableVal t -> evalTableOp op e1 e2
                                _ ->
                                  case H.lookup op runtime of
                                    Just (PrimBinop f)  -> f v1 v2
                                    Just _              -> return $ StrVal "not a Binary operator"
                                    Nothing             -> return $ StrVal "operator doesn't exist"

-- unary operations
eval (UnopExp op e) = do v <- eval e
                         case H.lookup op runtime of
                          Just (PrimUnop f)  -> f v
                          Just _             -> return $ StrVal "not a Unary operator"
                          Nothing            -> return $ StrVal "operator doesn't exist"


-- table expressions  
eval (TableConstructor fieldExpList) = do fieldValList <- mapM evalPair fieldExpList
                                          return $ TableVal $ H.fromList fieldValList

eval (TableLookUpExp varExp keyExp) =
      do keyVal <- eval keyExp
         tableVal <- eval varExp
         case tableVal of
           TableVal t -> case H.lookup keyVal t of
                          Just v  -> return v
                          Nothing -> return NilVal
           _          -> return $ StrVal "attempting to index a value that's not a table."

eval (FuncCallExp func args) =
  do funcVal <- eval func
     case funcVal of
       FuncVal params body clenv -> apply (FuncVal params body clenv) args
       _ -> return $ StrVal ("function name not found: " ++ (show func))


eval (MethodCallExp tableExp method args) =
  do tableVal <- eval tableExp
     fval <- locateMethod tableVal method
     case fval of
       (FuncVal params body clenv) -> apply (FuncVal params body clenv) (tableExp:args)
       (StrVal s) -> return $ StrVal s
       _ -> return $ StrVal ("method " ++ method ++ "not found")

locateMethod :: Val -> String -> EvalState Val
locateMethod tableVal method =
  do env <- get
     case tableVal of
       TableVal t ->
           case H.lookup (StrVal method) t of
               Just (FuncVal params body clenv) ->
                     return $ FuncVal params body clenv
               _ -> case H.lookup (StrVal "__metatable") t of
                       Just mt -> locateMethod mt method
                       _ -> return $ StrVal ("meta table not found")
       _ -> return $ StrVal "attempting to invoke on method from a value that's not a table"

evalTableOp :: String -> Exp -> Exp -> EvalState Val
evalTableOp op e1 e2 = do tableVal <- eval e1
                          case H.lookup op builtinOpMeta of
                            Just method ->
                                   do fval <- locateMethod tableVal method
                                      case fval of
                                        FuncVal params body clenv -> apply (FuncVal params body clenv) [e1,e2]
                                        (StrVal s) -> return $ StrVal s
                                        _ -> return $ StrVal ("operator metamethod " ++ op ++ " not found")
                            _ -> return $ StrVal "unrecognized operator"

opMetaMethods :: [(String, String)]
opMetaMethods = [("+", "__add"), ("-", "__sub")]

type MetaMethodEnv = H.HashMap String String
builtinOpMeta :: MetaMethodEnv
builtinOpMeta = H.fromList opMetaMethods

-- eval e = return $ StrVal ("unrecognized expression" ++ (show e))

apply :: Val -> [Exp] -> EvalState Val
apply (FuncVal params body clenv) args = do
    env <- get
    modify $ H.union clenv
    argvs <- mapM eval args
    mapM_ (\(k,v) -> modify $ H.insert k v ) (zip params argvs)
    val <- exec body
    put env -- restore original environment
    return val


{-
the list of values is adjusted to the length of the list of variables. 
If there are more values than needed, the excess values are thrown away. 
If there are fewer values than needed, the list is extended with nil's.
-}
myZip:: [Exp] -> [Val] -> [(Exp,Val)]
myZip varList valList = aux varList valList [] 
  where aux [] _ acc = acc  -- more val than needed 
        aux (x:xs) (y:ys) acc =  aux xs ys ((x,y):acc)
        aux (x:xs) [] acc = aux xs [] ((x, NilVal):acc) -- fewer values than needed pad with nil's 


exec :: Stmt -> EvalState Val

exec NullStmt = return NilVal

exec (PrintStmt e) = do v <- eval e 
                        return $ StrVal (show v)

exec (ReturnStmt e) = do v <- eval e
                         return $ v

-- key expression could be VarExp or TableLookUpExp 
exec (AssignStmt keyExpList valExpList) = do
     valList <- mapM eval valExpList 
     let f = (\((VarExp s),v) ->  modify $ H.insert s v)
     mapM f $ myZip keyExpList valList 
     return $ NilVal

exec (TableAssignStmt e1@(VarExp tableName) e2 e3) = do 
      oldTableVal <- eval e1 
      case oldTableVal of 
        (TableVal oldTable) -> do keyVal   <- eval e2 
                                  newVal   <- eval e3 
                                  let newTable = H.insert keyVal newVal oldTable
                                  modify $ H.insert tableName (TableVal newTable)
                                  return $ NilVal
        _                   -> return $ StrVal "attempting to index a value that's not a table."

exec (SeqStmt xs) = do outputs <- mapM exec xs
                       return $ last outputs

exec (FuncStmt fname params body) = do
  env <- get
  val <- (\argVal -> FuncVal argVal body env) <$> mapM getName params
  modify $ H.insert fname val
  return $ NilVal

exec (MethodStmt tname (FuncStmt fname params body)) = do
  env <- get
  tableVal <- eval (VarExp tname)
  case tableVal of
    (TableVal t) -> do fval <- (\argVal -> FuncVal argVal body env) <$> mapM getName params
                       let newTable = H.insert (StrVal fname) fval t
                       modify $ H.insert tname (TableVal newTable)
                       return $ NilVal
    _ -> return $ StrVal ("table " ++ tname ++ " not found")

exec (IfStmt exp s1 s2) = do
  cond <- eval exp
  case cond of
    (BoolVal b) | b == True -> do val <- exec s1
                                  return val
    (BoolVal b) -> do val <- exec s2
                      return val
    (IntVal i) | i == 0 -> do val <- exec s2
                              return val
    (IntVal i) -> do val <- exec s1
                     return val
    _ -> return $ StrVal "IF condition could not evaluate to boolean value"

exec BreakStmt = return $ ControlVal "break"

exec (ForStmt fvar start end step body) = do
  startVal <- eval start
  endVal <- eval end
  stepVal <- eval step
  case (startVal, endVal, stepVal) of
    (IntVal i1, IntVal i2, IntVal 0) -> return $ StrVal "FOR step expression evaluates to zero"
    (IntVal i1, IntVal i2, IntVal i3) -> do modify $ H.insert fvar startVal
                                            val <- loopExec fvar endVal stepVal body ""
                                            modify $ H.delete fvar  -- control variable would not be accessible outside loop
                                            return val
    _ -> return $ StrVal "FOR control expression evaluates to none-numerical value"

exec s = return  $ StrVal ("invalid statement" ++ (show s))

loopExec :: String -> Val -> Val -> Stmt -> String -> EvalState Val
loopExec var end step body outputs =
  do
    condVal <- loopCond var end step
    case condVal of
      (BoolVal False) -> return $ StrVal outputs
      _ -> do val <- exec body
              case val of
                (ControlVal "break") -> return $ StrVal outputs
                (StrVal s) -> do nval <- loopStep var step
                                 loopExec var end step body (outputs ++ "\n" ++ s)
                _ -> do nval <- loopStep var step
                        loopExec var end step body outputs

loopCond :: String -> Val -> Val -> EvalState Val
loopCond var (IntVal end) step =
   do curVal <- eval (VarExp var)
      case (curVal, step) of
        (IntVal cur, IntVal s) | s > 0 -> return $ BoolVal (cur < end)
        (IntVal cur, IntVal s) -> return $ BoolVal (cur > end)
        _ -> return $ BoolVal False

loopStep :: String -> Val -> EvalState Val
loopStep var (IntVal step) =
  do curVal <- eval (VarExp var)
     case curVal of
       (IntVal cur) -> do modify $ H.insert var (IntVal (cur+step))
                          return $ IntVal (cur+step)
       _ -> return $ NilVal
