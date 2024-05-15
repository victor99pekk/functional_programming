module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
-- data Statement =
--     Assignment String Expr.T |
--     If Expr.T Statement Statement
--     deriving Show

data Statement =

    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter Expr.parse #- require "end" >-> buildBegin
buildBegin stmts = Begin stmts

-- ifStmt = accept "if" -# Expr.parse #- require "then" -# assignment #- require "else" -# assignment >-> buildIf
-- buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt
-- ifStmt = accept "if" -# Expr.parse #- require "then" -# parse #- require "else" -# parse >-> buildIf

ifStmt = accept "if" -# Expr.parse # require "then" -# parse # require "else" -# parse >-> buildIf
buildIf ((cond, thenStmt), elseStmt) = If cond thenStmt elseStmt

whileStmt = accept "while" -# Expr.parse # require "do" -# parse >-> buildWhile
buildWhile (cond, stmt) = While cond stmt


readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

comment = accept "-- " -# iter (char ? (/= '\n')) #- require "\n" >-> buildComment
-- commentParser = accept "--" -# iter (char ? (/= '\n')) #- require "\n" 
buildComment s = Comment s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond stmt: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (stmt: While cond stmt: stmts) dict input
    else exec stmts dict input
exec (Assignment v e: stmts) dict input = exec stmts (Dictionary.insert (v, Expr.value e dict) dict) input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Begin stmts': stmts) dict input = exec (stmts' ++ stmts) dict input
exec (Read v: stmts) dict (i:input) = exec stmts (Dictionary.insert (v, i) dict) input
exec (Write e: stmts) dict input = Expr.value e dict : exec stmts dict input
exec (Comment _: stmts) dict input = exec stmts dict input
exec _ _ _ = []


indent :: String -> String
indent str = unlines . map ("\t" ++) . lines $ str

instance Parse Statement where
  parse = comment ! assignment ! skip ! begin ! ifStmt ! whileStmt ! readStmt ! write
  toString s = case s of
    Assignment v e -> v ++ " := " ++ Expr.toString e ++ ";\n"
    Skip -> "skip;\n"
    Begin stmts -> "begin\n" ++ indent (concatMap toString stmts) ++ "end"
    Write e -> "write " ++ Expr.toString e ++ ";\n"
    Read e -> "read " ++ e ++ ";\n"
    If cond thenStmt elseStmt -> "if " ++ Expr.toString cond ++ " then\n" ++ indent (toString thenStmt) ++ "else\n" ++ indent (toString elseStmt) ++ "\n"
    While cond stmt -> "while " ++ Expr.toString cond ++ " do\n" ++ indent (toString stmt)
    Comment c -> "-- " ++ c ++ "\n"
