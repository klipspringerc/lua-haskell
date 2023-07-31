module Lua.Parse where

import Lua.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Text.Parsec.Token hiding (parens, symbol)
import Text.Parsec.Expr 
import Control.Monad



type Parser = ParsecT String () Identity

-- string match multi character patterns
-- many tries parser until failure and return result list
-- try tries a parser that does not consume input at failure
-- seqby tries 2 parser alternatively
-- noneof consume charactors excluding the given char set


-- Lexicals 

symbol :: String -> Parser String
symbol s = do spaces
              string s
              spaces
              return s

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

int :: Parser Int
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var :: Parser String
var = do v <- many1 letter <?> "an identifier"
         spaces
         return v


-- Expressions
nilExp :: Parser Exp 
nilExp =  symbol "nil" >> return (NilExp)

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

boolExp :: Parser Exp
boolExp =    ( symbol "true"  >> return (BoolExp True)  )
         <|> ( symbol "false" >> return (BoolExp False) )

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

strExp :: Parser Exp 
strExp = do char '"'
            s <- many $ noneOf ['"']
            char '"'
            spaces
            return $ StrExp s


fieldSep :: Parser String 
fieldSep = (symbol ",") <|> (symbol ";")

--currently only support the most general form of table constructor  {[Exp] = Exp }
tableConstructor :: Parser Exp   
tableConstructor = do try $ symbol "{"
                      fieldList <- (do symbol "["
                                       keyExp <- expr
                                       symbol "]"
                                       symbol "="
                                       valExp <- expr
                                       return (keyExp, valExp)
                                    )
                                    `sepBy` fieldSep
                      symbol "}"
                      return $ TableConstructor fieldList

tableLookUpExp :: Parser Exp 
tableLookUpExp = do t <- var 
                    symbol "["
                    keyExp <- expr 
                    symbol "]"
                    return $ TableLookUpExp (VarExp t) keyExp

tableAltLookUpExp :: Parser Exp
tableAltLookUpExp = do t <- var
                       symbol "."
                       key <- var
                       return $ TableLookUpExp (VarExp t) (StrExp key)

funcCallExp :: Parser Exp
funcCallExp = do fname <- var
                 symbol "("
                 argList <- expr `sepBy` (symbol ",")
                 symbol ")"
                 return $ FuncCallExp (VarExp fname) argList

methodCallExp :: Parser Exp
methodCallExp = do tname <- var
                   symbol ":"
                   fname <- var
                   symbol "("
                   argList <- expr `sepBy` (symbol ",")
                   symbol ")"
                   return $ MethodCallExp (VarExp tname) fname argList

expr :: Parser Exp
expr = try (buildExpressionParser table atom)  <|> atom <?> "expression"

atom :: Parser Exp
atom = try nilExp 
   <|> try intExp
   <|> try boolExp  
   <|> try strExp
   <|> try tableLookUpExp
   <|> try tableAltLookUpExp
   <|> try tableConstructor
   <|> try methodCallExp
   <|> try funcCallExp
   <|> try varExp
   <|> parens expr


{-
Precedence and associativity 
or
and
<     >     <=    >=    ~=    ==
|
~
&
<<    >>
..
+     -
*     /     //    %
unary operators (not   #     -     ~)
^
-}
--  
-- fun should be Exp -> Exp -> Exp 
binary  name fun assoc = Infix (do{ try (symbol name); return fun }) assoc
prefix  name fun       = Prefix (do{ try (symbol name); return fun })
table = [ [binary "^" (BinopExp "^") AssocRight]
        , [prefix op (UnopExp ("unop"++op)) | op <- ["not", "#", "-"]]
        , [binary op (BinopExp op) AssocLeft | op <- ["*","/","//","%"]]
        , [binary op (BinopExp op) AssocLeft | op <- ["+","-"]]
        , [binary ".." (BinopExp "..") AssocRight]
        , [binary op (BinopExp op) AssocLeft | op <- ["<=",">=","~=","==","<",">"]]
        , [binary "and" (BinopExp "and") AssocLeft]
        , [binary "or" (BinopExp "or") AssocLeft]]


printStmt :: Parser Stmt
printStmt = do symbol "print"
               e <- parens expr 
               return $ PrintStmt e

quitStmt :: Parser Stmt
quitStmt = do symbol "quit"
              return QuitStmt

assignStmt :: Parser Stmt 
assignStmt = do keyExpList <- varExp `sepBy` (symbol ",")
                symbol "="
                valExpList <- expr `sepBy` (symbol ",")
                return $ AssignStmt keyExpList valExpList
tableAssignStmt ::Parser Stmt 
tableAssignStmt = do e1 <- varExp 
                     symbol "["
                     e2 <- expr 
                     symbol "]"
                     symbol "="
                     e3 <- expr 
                     return $ TableAssignStmt e1 e2 e3 

seqStmt :: Parser Stmt 
seqStmt = do symbol "do"
             stmtList <- stmt `sepBy` (symbol ";") 
             symbol "end"
             return $ SeqStmt stmtList

retStmt :: Parser Stmt
retStmt = do symbol "return"
             valExp <- expr
             return $ ReturnStmt valExp

funcStmt :: Parser Stmt
funcStmt = do symbol "function"
              fname <- var
              symbol "("
              paramList <- varExp `sepBy` (symbol ",")
              symbol ")"
              spaces
              body <- stmt
              spaces
              symbol "end"
              return $ FuncStmt fname paramList body

methodStmt :: Parser Stmt
methodStmt = do symbol "function"
                tname <- var
                symbol ":"
                fname <- var
                symbol "("
                paramList <- varExp `sepBy` (symbol ",")
                symbol ")"
                spaces
                body <- stmt
                spaces
                symbol "end"
                let params = (VarExp "self"):paramList -- add self to parameter list
                return $ MethodStmt tname (FuncStmt fname params body)

ifStmt :: Parser Stmt
ifStmt = do symbol "if"
            exp <- expr
            symbol "then"
            s1 <- stmt
            s2 <- ifCont
            return $ IfStmt exp s1 s2

ifEndStmt :: Parser Stmt
ifEndStmt = symbol "end" >> return NullStmt

ifElseStmt :: Parser Stmt
ifElseStmt = do symbol "else"
                s <- stmt
                symbol "end"
                return s

-- Handle `elseif` as recursive IfStmt
ifContStmt :: Parser Stmt
ifContStmt = do symbol "elseif"
                exp <- expr
                symbol "then"
                s1 <- stmt
                s2 <- ifCont
                return $ IfStmt exp s1 s2

ifCont :: Parser Stmt
ifCont = try ifEndStmt
      <|> try ifContStmt
      <|> ifElseStmt

breakStmt :: Parser Stmt
breakStmt = symbol "break" >> return BreakStmt

forStmt :: Parser Stmt
forStmt = do symbol "for"
             fvar <- var
             symbol "="
             startExp <- expr
             symbol ","
             endExp <- expr
             stepExp <- optForStep
             symbol "do"
             body <- stmt
             symbol "end"
             return $ ForStmt fvar startExp endExp stepExp body

-- for loop step value is optional, defaults to 1
forStepExp :: Parser Exp
forStepExp = do symbol ","
                exp <- expr
                return exp

defaultStepExp :: Parser Exp
defaultStepExp = do spaces
                    return (IntExp 1)

optForStep :: Parser Exp
optForStep = try forStepExp <|> defaultStepExp

whileStmt :: Parser Stmt
whileStmt = do symbol "while"
               exp <- expr
               symbol "do"
               body <- stmt
               symbol "end"
               return $ WhileStmt exp body

setMetaStmt :: Parser Stmt
setMetaStmt = do symbol "setmetatable"
                 symbol "("
                 tname <- var
                 symbol ","
                 mname <- var
                 symbol ")"
                 return $ SetMetaStmt tname mname

expStmt :: Parser Stmt
expStmt = do exp <- expr
             return $ ReturnStmt exp

stmt :: Parser Stmt
stmt =  try quitStmt 
    <|> try seqStmt
    <|> try printStmt
    <|> try retStmt
    <|> try ifStmt
    <|> try forStmt
    <|> try whileStmt
    <|> try breakStmt
    <|> try tableAssignStmt 
    <|> try assignStmt
    <|> try methodStmt
    <|> try funcStmt
    <|> expStmt
