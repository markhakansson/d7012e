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

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
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

parseRead = ((spaces # accept "read")) -# word >-> buildRead
buildRead var = Read var

parseWrite = ((spaces # accept "read")) -# Expr.parse >-> buildWrite
buildWrite e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
--exec (Read var) dict input =

instance Parse Statement where
  parse = 
    assignment !
    parseSkip !
    parseBegin !
    parseIf !
    parseWhile !
    parseRead !
    parseWrite
  toString = error "Statement.toString not implemented"
