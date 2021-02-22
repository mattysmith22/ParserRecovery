module Triangle.Parser (parseProgram) where

import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.ParserRecovery
import           Triangle.AST


type Parser c a = RecoveryParserT c Void String Identity a

tok :: Parser c a -> Parser c a
tok p = p <* space

parens :: a -> Parser c a -> Parser c a
parens def p = fromMaybe def <$> betweenSync (symbol "(") (symbol ")") p

ident :: Parser c String
ident = tok ((:) <$> alpha <*> many alphaNum)
    where
        alpha = letterChar <|> char '_'
        alphaNum = alpha <|> digitChar

term :: Parser c AST
term = tok (Val <$> L.decimal)  <|> (Var <$> ident) <|> parens (Val 0) expr

symbol :: String -> Parser c String
symbol = L.symbol space

expr :: Parser c AST
expr = makeExprParser term opers
    where
        opers =[
            [Prefix (UnOp <$> unaryTok)]
            ,[InfixL (BinOp <$> mulDivTok)]
            ,[InfixL (BinOp <$> addSubTok)]
            ,[InfixL (BinOp <$> compareTok)]
            ,[InfixL (BinOp <$> andTok)]
            ,[InfixL (BinOp <$> orTok)]
            ,[TernR $ (Cond <$ symbol ":") <$ symbol "?"]
            ]
        unaryTok = (Not <$ symbol "!") <|> (Neg <$ symbol "-")
        addSubTok = (Add <$ tok (char '+')) <|> (Sub <$ tok (char '-'))
        mulDivTok = (Mul <$ tok (char '*')) <|> (Div <$ tok (char '/'))
        andTok = And <$ symbol "&&"
        orTok = Or <$ symbol "||"
        compareTok = (Eq <$ symbol "==") <|> (Neq <$ symbol "!=")
            <|> (Le <$ symbol "<=") <|> (Lt <$ symbol "<")
            <|> (Ge <$ symbol ">=") <|> (Gt <$ symbol ">")

tokIdent :: Parser c String
tokIdent = do
    idnt <- ident
    if idnt == "end" then fail "" else return idnt

program :: Parser c Program
program = Program <$ symbol "let" <*> declarations <* symbol "in" <*> command

declaration :: Parser c Declaration
declaration = Declaration <$ symbol "var" <*> tokIdent <*> initValue
    where
        initValue = (Just <$ symbol ":=" <*> expr) <|> return Nothing

declarations :: Parser c [Declaration]
declarations = sepBySync declaration (symbol ";")

command :: Parser c Command
command =
        (If <$ symbol "if" <*> expr <* symbol "then" <*> command <* symbol "else" <*> command)
    <|> (While <$ symbol "while" <*> expr <* symbol "do" <*> command)
    <|> (GetInt <$ symbol "getint" <*> parens "" ident)
    <|> (PrintInt <$ symbol "printint" <*> parens (Var "") expr)
    <|> (Block <$> fromMaybe [] <$> betweenSync (symbol "begin") (symbol "end") commands)
    <|> (Assign <$> tokIdent <* symbol ":=" <*> expr)

commands :: Parser c [Command]
commands = sepBySync command (symbol ";")

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram s = runIdentity $ runRecoveryParser (space >> program) "" s
