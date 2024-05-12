module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T] -- Program represented as a list of Statements

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = error "Program.toString not implemented"

exec (Program stmts) = Statement.exec stmts Dictionary.empty []