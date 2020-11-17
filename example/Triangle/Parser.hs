module Triangle.Parser (parseProgram, declaration, declarations) where

import           Data.Void
import qualified Expr.Frontend        as Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Triangle.AST

type Parser a = Parsec Void String a

tok :: Parser a -> Parser a
tok p = p <* space

tokLet :: Parser String
tokLet = tok $ string "let"
tokIn :: Parser String
tokIn = tok $ string "in"
tokVar :: Parser String
tokVar = tok $ string "var"
tokEql :: Parser String
tokEql = tok $ string ":="
tokSemicolon :: Parser Char
tokSemicolon = tok $ char ';'
tokIf :: Parser String
tokIf = tok $ string "if"
tokThen :: Parser String
tokThen = tok $ string "then"
tokElse :: Parser String
tokElse = tok $ string "else"
tokWhile :: Parser String
tokWhile = tok $ string "while"
tokDo :: Parser String
tokDo = tok $ string "do"
tokGetInt :: Parser String
tokGetInt = tok $ string "getint"
tokPrintInt :: Parser String
tokPrintInt = tok $ string "printint"
tokOpenP :: Parser Char
tokOpenP = tok $ char '('
tokCloseP :: Parser Char
tokCloseP = tok $ char ')'
tokBegin :: Parser String
tokBegin = tok $ string "begin"
tokEnd :: Parser String
tokEnd = tok $ string "end"

tokIdent :: Parser String
tokIdent = do
    idnt <- Expr.ident
    if idnt == "end" then fail "" else return idnt

program :: Parser Program
program = Program <$ tokLet <*> declarations <* tokIn <*> command

declaration :: Parser Declaration
declaration = Declaration <$ tokVar <*> tokIdent <*> initValue
    where
        initValue = (Just <$ tokEql <*> Expr.expr) <|> return Nothing

declarations :: Parser [Declaration]
declarations = sepBy1 declaration tokSemicolon
command :: Parser Command
command =
        (If <$ tokIf <*> Expr.expr <* tokThen <*> command <* tokElse <*> command)
    <|> (While <$ tokWhile <*> Expr.expr <* tokDo <*> command)
    <|> (GetInt <$ tokGetInt <*> between tokOpenP tokCloseP Expr.ident)
    <|> (PrintInt <$ tokPrintInt <*> between tokOpenP tokCloseP Expr.expr)
    <|> (Block <$> between tokBegin tokEnd commands)
    <|> (Assign <$> tokIdent <* tokEql <*> Expr.expr)

commands :: Parser [Command]
commands = sepBy command tokSemicolon

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse (space >> program) ""
