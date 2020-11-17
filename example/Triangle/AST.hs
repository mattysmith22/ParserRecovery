module Triangle.AST (Ident, Program(..), Declaration(..), Command(..)) where

import           Expr.AST (AST)

type Ident = String

data Program = Program {
    pDeclarations :: [Declaration],
    pCommand      :: Command
} deriving (Eq, Show)

data Declaration = Declaration {
    ident :: Ident,
    value :: Maybe AST
} deriving (Eq, Show)

data Command = Assign Ident AST
    | If AST Command Command
    | While AST Command
    | GetInt Ident
    | PrintInt AST
    | Block [Command]
    deriving (Eq, Show)
