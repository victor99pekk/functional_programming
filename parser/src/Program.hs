module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T] -- Program represented as a list of Statements

instance Parse T where
  parse :: Parser T
  parse = iter Statement.parse >-> Program

  toString :: T -> String
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts Dictionary.empty input

instance Show T where -- EXTRA
  show :: T -> String
  show = toString

