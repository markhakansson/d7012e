module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement

data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Repeat Statement Expr.T
    deriving Show

assignment = spaces -# word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

parseSkip = (spaces # accept "skip") #- require ";" >-> buildSkip
buildSkip _ = Skip

parseBegin = 
    (spaces # accept "begin") -# 
    iter parse 
    #- (spaces # require "end") >-> buildBegin 
buildBegin stmts = Begin stmts

parseIf = 
    ((spaces # accept "if") -# Expr.parse) # 
    ((spaces # require "then") -# parse) # 
    ((spaces # require "else") -# parse) >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

parseWhile = 
    ((spaces # accept "while") -# Expr.parse) #
    ((spaces # require "do") -# parse) >-> buildWhile
buildWhile (e, s) = While e s

parseRead = ((spaces # accept "read")) -# word #- require ";" >-> buildRead
buildRead var = Read var

parseWrite = ((spaces # accept "write")) -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

parseRepeat = 
    ((spaces # accept "repeat")) -# parse #
    ((spaces # require "until") -# 
    (spaces -# Expr.parse) #- 
    (spaces # require ";")) >-> buildRepeat
buildRepeat (s, e) = Repeat s e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment var expr: stmts) dict input = exec stmts updatedDict input
    where updatedDict = Dictionary.insert (var, (Expr.value expr dict)) dict

exec (Skip: stmts) dict input = exec stmts dict input

exec (Begin innerStmts: outerStmts) dict input = 
    exec (innerStmts ++ outerStmts) dict input

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict) > 0 then 
        exec (thenStmts : stmts) dict input
    else
         exec (elseStmts : stmts) dict input

exec (While cond doStmts : stmts) dict input =
    if (Expr.value cond dict) > 0 then 
        exec (doStmts : While cond doStmts : stmts) dict input
    else 
        exec stmts dict input

exec (Read var : stmts) dict input = exec stmts updatedDict updatedInput
    where 
        updatedDict = Dictionary.insert (var, head input) dict
        updatedInput = tail input

exec (Write expr : stmts) dict input = (Expr.value expr dict) : exec stmts dict input

exec (Repeat stmt cond : stmts) dict input =
    exec (stmt : if (Expr.value cond dict) < 0 then Repeat stmt cond: stmts else stmts) dict input
exec _ _ _ = []

-- Converts Statements to Strings
shw :: Integer -> Statement -> String
shw i (Assignment var expr) = 
    tab i ++ var ++ " := " ++ (Expr.toString expr) ++ ";\n"
shw i (Skip) = 
    tab i ++ "skip;\n"
shw i (Begin stmts) = 
    tab i ++ "begin\n"  ++ (foldl (++) "" (map (shw (i+1)) stmts)) ++ tab i ++ "end\n"
shw i (If cond thenStmt elseStmt) = 
    tab i ++ "if " ++ (Expr.toString cond) ++ " then\n" ++ 
    shw (i+1) thenStmt ++ tab i ++ "else\n" ++ shw (i+1) elseStmt
shw i (While cond doStmts) = 
    tab i ++ "while " ++ (Expr.toString cond) ++ " do\n" ++ shw (i+1) doStmts
shw i (Read var) = 
    tab i ++ "read " ++ var ++ ";\n"
shw i (Write expr) = 
    tab i ++ "write " ++ (Expr.toString expr) ++ ";\n"
shw i (Repeat stmt expr) = 
    tab i ++ "repeat\n" ++ shw(i+1) stmt ++ tab i ++ "until " ++ (Expr.toString expr) ++ ";\n"

-- Create i number of tabs (here tab = 4 spaces)
tab :: Integer -> String
tab 0 = ""
tab 1 = "    "
tab i = "    " ++ tab (i-1)

instance Parse Statement where
  parse = 
    assignment !
    parseSkip !
    parseBegin !
    parseIf !
    parseWhile !
    parseRead !
    parseWrite !
    parseRepeat
  toString = shw 0
