{-# LANGUAGE TypeFamilies #-}
module Expr.Frontend (parseInput, parseInt, expr, ident) where

import           Data.Function
import           Data.Maybe
import           Data.Void
import           Expr.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.ParserRecovery

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl pX pOp = pX >>= remain
    where
        remain l = (do
                op <- pOp
                r <- pX
                remain (op l r))
            <|> return l

type Parser a = Parsec Void String a

-- Removes spaces after parsed token.
tok :: Parser a -> Parser a
tok p = p <* space

addSub :: Parser BinOp
addSub = (Add <$ tok (char '+')) <|> (Sub <$ tok (char '-'))

mulDiv :: Parser BinOp
mulDiv = (Mul <$ tok (char '*')) <|> (Div <$ tok (char '/'))

andT :: Parser BinOp
andT = And <$ tok (string "&&")

orT :: Parser BinOp
orT = Or <$ tok (string "||")

relation :: Parser BinOp
relation = (Eq <$ (tok $ string "==")) <|> (Neq <$ (tok $ string "!="))
    <|> (Le <$ (tok $ string "<=")) <|> (Lt <$ (tok $ char '<'))
    <|> (Ge <$ (tok $ string ">=")) <|> (Gt <$ (tok $ char '>'))

openP :: Parser Char
openP = tok (char '(')

closeP :: Parser Char
closeP = tok (char ')')

neg :: Parser UnOp
neg = Neg <$ tok (char '-')

ifExp :: Parser AST
ifExp = pure (&) <*> orExp <*> trail
    where
        trail = (pure (\l r c -> Cond c l r) <* question <*> orExp <* colon <*> orExp) <|> return id
        question = tok (char '?')
        colon = tok (char ':')

orExp :: Parser AST
orExp = do
    l <- andExp
    f <- option id $ do
        op <- orT
        r <- orExp
        return (\l -> BinOp op l r)
    return (f l)

andExp :: Parser AST
andExp = do
    l <- notExp
    f <- option id $ do
        op <- andT
        r <- andExp
        return (\l -> BinOp op l r)
    return (f l)

notExp :: Parser AST
notExp = (pure UnOp <*> not <*> notExp) <|> relExp
    where
        not = Not <$ (tok (char '!'))

relExp :: Parser AST
relExp = do
    l <- addExp
    f <- option id $ do
        op <- relation
        r <- relExp
        return (\l -> BinOp op l r)
    return (f l)

addExp :: Parser AST
addExp = do
    chainl mExpr oper
    where
        oper = BinOp <$> addSub

mExpr :: Parser AST
mExpr = do
    chainl term oper
    where
        oper = BinOp <$> mulDiv

int :: Parser AST
int = Val . (read::String -> Int) <$> tok (some digitChar)

literal :: Parser AST
literal = Var <$> ident

ident :: Parser String
ident = tok ((:) <$> alpha <*> many alphaNum)
    where
        alpha = letterChar <|> char '_'
        alphaNum = alpha <|> digitChar

term :: Parser AST
term = (UnOp <$> neg <*> term) <|> int  <|> literal <|> (fromMaybe (Val 0)) <$> (betweenSync openP closeP expr)

expr :: Parser AST
expr = ifExp

parseInput :: String -> Either (ParseErrorBundle String Void) AST
parseInput = parse (space >> expr) ""

parseInt :: String -> Either (ParseErrorBundle String Void) Int
parseInt = parse (space >> ((read::String -> Int) <$> tok (some digitChar))) ""
