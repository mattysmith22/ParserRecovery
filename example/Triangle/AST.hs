module Triangle.AST (Ident, Program(..), Declaration(..), Command(..), BinOp(..), UnOp(..), AST(..)) where

data BinOp = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
    deriving (Show, Eq)
data UnOp = Neg | Not
    deriving (Show, Eq)

data AST = BinOp BinOp AST AST
    | UnOp UnOp AST
    | Val Int
    | Var String
    | Cond AST AST AST
    deriving (Show, Eq)

type Ident = String

data Program = Program {
    pDeclarations :: [Declaration],
    pCommand      :: Command
} deriving (Eq, Show)

data Declaration = Declaration {
    dIdent :: Ident,
    dValue :: Maybe AST
} deriving (Eq, Show)

data Command = Assign Ident AST
    | If AST Command Command
    | While AST Command
    | GetInt Ident
    | PrintInt AST
    | Block [Command]
    deriving (Eq, Show)
