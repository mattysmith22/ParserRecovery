module Triangle.AST (Ident, Program, Declaration(..), Command(..), Operator(..), AST(..)) where

data Operator = Add | Sub | Mul | Div | Lt | Gt | Eq | Not | Neg
    deriving (Show, Eq)

data AST = BinOp Operator AST AST
    | UnOp Operator AST
    | Val Int
    | Var String
    deriving (Show, Eq)

type Ident = String

type Program = Command

data Declaration = Variable Ident AST
    | Const Ident AST
    deriving (Show, Eq)

data Command = Assign Ident AST
    | Call Ident AST
    | If AST Command Command
    | While AST Command
    | LetDeclaration [Declaration] Command
    | Block [Command]
    deriving (Eq, Show)
