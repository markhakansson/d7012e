module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T] deriving Show

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = toStringHelper

toStringHelper :: T -> String
toStringHelper (Program stmts) = foldl (++) "" (map Statement.toString stmts)
             
exec (Program stmts) =  Statement.exec stmts Dictionary.empty 