module Expr.AST(BinOp(..), UnOp(..), AST(..), commutative, applyUnOp, applyBinOp, requiredVariables, numToBool, boolToNum) where

import           Data.List

data BinOp = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
    deriving (Show, Eq)
data UnOp = Neg | Not
    deriving (Show, Eq)

data AST = BinOp BinOp AST AST
    | UnOp UnOp AST
    | Val Int
    | Var String
    | Cond AST AST AST
    deriving Show

commutative :: BinOp -> Bool
commutative Add = True
commutative Mul = True
commutative And = True
commutative Or  = True
commutative Eq  = True
commutative Neq = True
commutative _   = False

instance Eq AST where
    (Val l) == (Val r) = l == r
    (Var l) == (Var r) = l == r
    (UnOp opl l) == (UnOp opr r) = (opl == opr) && (l == r)
    (BinOp opl l1 l2) == (BinOp opr r1 r2)
        | opl == opr && commutative opl = (l1 == r1 && l2 == r2) || (l1 == r2 && l2 == r1)
        | opl == opr = l1== r1 && l2 == r2
        | otherwise = False
    (Cond c1 l1 r1) == (Cond c2 l2 r2) = c1 == c2 && l1 == l2 && r1 == r2
    _ == _ = False

applyUnOp :: UnOp -> (Int -> Int)
applyUnOp Neg x = -x
applyUnOp Not x = boolToNum $ not $ numToBool x

numToBool :: Int -> Bool
numToBool 0 = False
numToBool _ = True

boolToNum :: Bool -> Int
boolToNum False = 0
boolToNum True  = 1

applyBinOp :: BinOp -> Int -> Int -> Int
applyBinOp Add = (+)
applyBinOp Sub = (-)
applyBinOp Mul = (*)
applyBinOp Div = div
applyBinOp Eq  = \l r -> boolToNum (l == r)
applyBinOp Lt  = \l r -> boolToNum (l < r)
applyBinOp Gt  = \l r -> boolToNum (l > r)
applyBinOp And = \l r -> boolToNum (numToBool l && numToBool r)
applyBinOp Or  = \l r -> boolToNum (numToBool l || numToBool r)
applyBinOp Neq = \l r -> boolToNum (l /= r)
applyBinOp Le  = \l r -> boolToNum (l <= r)
applyBinOp Ge  = \l r -> boolToNum (l >= r)


requiredVariables :: AST -> [String]
requiredVariables = nub . requiredVariablesDup

requiredVariablesDup :: AST -> [String]
requiredVariablesDup (Var x) = [x]
requiredVariablesDup (Val _) = []
requiredVariablesDup (UnOp _ x) = requiredVariablesDup x
requiredVariablesDup (BinOp _ l r) = requiredVariablesDup l ++ requiredVariablesDup r
requiredVariablesDup (Cond c t f) = requiredVariablesDup c ++ requiredVariablesDup t ++ requiredVariablesDup f
