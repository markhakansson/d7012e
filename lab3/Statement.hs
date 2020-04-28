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
    Write Expr.T
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

exec _ _ _ = []

-- Converts Statements to Strings
shw :: Statement -> String
shw (Assignment var expr) = var ++ " := " ++ (Expr.toString expr) ++ ";\n"
shw (Skip) = "skip;\n"
shw (Begin stmts) = "begin\n"  ++ (foldl (++) "" (map shw stmts)) ++ "end\n"
shw (If cond thenStmt elseStmt) = "if " ++ (Expr.toString cond) ++ " then\n" ++ shw thenStmt ++ "else\n" ++ shw elseStmt
shw (While cond doStmts) = "while " ++ (Expr.toString cond) ++ " do\n" ++ shw doStmts
shw (Read var) = "read " ++ var ++ ";\n"
shw (Write expr) = "write " ++ (Expr.toString expr) ++ ";\n"

-- Create i number of tabs (for indentation)
tab :: Integer -> String
tab 0 = ""
tab 1 = "\t"
tab i = "\t" ++ tab (i-1)

instance Parse Statement where
  parse = 
    assignment !
    parseSkip !
    parseBegin !
    parseIf !
    parseWhile !
    parseRead !
    parseWrite
  toString = shw
