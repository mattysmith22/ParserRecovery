module Triangle.Parser (parseProgram) where

import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
--import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.ParserRecovery
import           Triangle.AST
import Data.Functor

type Parser a = RecoveryParserT Void String Identity a

whitespace :: Parser ()
whitespace = void $ many (void spaceChar <|> void comment)
    where
        comment = char '!' >> manyTill anySingle (char '\n')

withDefault :: Functor f => a -> f (Maybe a) -> f a
withDefault def = fmap (fromMaybe def)

tok :: Parser a -> Parser a
tok = L.lexeme whitespace

parens :: a -> Parser a -> Parser a
parens def p = fromMaybe def <$> betweenSync (symbol "(") (symbol ")") p

ident :: Parser String
ident = tok ((:) <$> alpha <*> many alphaNum)
    where
        alpha = letterChar <|> char '_'
        alphaNum = alpha <|> digitChar

term :: Parser AST
term = tok (Val <$> L.decimal)  <|> (Var <$> tokIdent) <|> parens (Val 0) expr

symbol :: String -> Parser String
symbol = L.symbol whitespace

expr :: Parser AST
expr = makeExprParser term opers
    where
        opers =[
            [Prefix (UnOp <$> unaryTok)]
            ,[InfixL (BinOp <$> mulDivTok)]
            ,[InfixL (BinOp <$> addSubTok)]
            ,[InfixL (BinOp <$> compareTok)]]
        unaryTok = (Not <$ symbol "\\") <|> (Neg <$ symbol "-")
        addSubTok = (Add <$ tok (char '+')) <|> (Sub <$ tok (char '-'))
        mulDivTok = (Mul <$ tok (char '*')) <|> (Div <$ tok (char '/'))
        compareTok = (Eq <$ symbol "=") <|> (Lt <$ symbol "<") <|> (Gt <$ symbol ">")

tokIdent :: Parser String
tokIdent = do
    idnt <- ident
    if idnt == "end" then fail "" else return idnt

program :: Parser Program
program = command

declaration :: Parser Declaration
declaration =
    Variable <$> withDefault "" (betweenSync (symbol "var") (symbol ":") tokIdent) <*> expr
    <|> Const <$> withDefault "" (betweenSync (symbol "const") (symbol "~") tokIdent) <*> expr

declarations :: Parser [Declaration]
declarations = sepBySync declaration (symbol ";")

command :: Parser Command
command =
        (If <$> withDefault (Var "") (betweenSync (symbol "if") (symbol "then") expr) <*> command <* symbol "else" <*> command)
    <|> (While <$ symbol "while" <*> expr <* symbol "do" <*> command)
    <|> (Block <$> withDefault [] (betweenSync (symbol "begin") (symbol "end") commands))
    <|> (LetDeclaration <$> withDefault [] (betweenSync (symbol "let") (symbol "in") declarations) <*> command)
    <|> (tokIdent >>= assignOrCall)
    where
        assignOrCall idnt = (Assign idnt <$ symbol ":=" <*> expr)
            <|> (Call idnt <$> withDefault (Var "") (betweenSync (symbol "(") (symbol ")") expr))

commands :: Parser [Command]
commands = sepBySync command (symbol ";")

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram s = runIdentity $ runRecoveryParser (whitespace >> program) "" s
