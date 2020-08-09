{-# LANGUAGE MultiParamTypeClasses #-}
module Text.ParserRecovery (

) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State.Lazy

type TextLocation = (Int, Int)

type ParseError = (ErrorMessage, TextLocation)

data ParserResult a =
    Succ a ParserState
    | Soft [ParseError] ParserState
    | Hard [ParseError]

instance Functor ParserResult where
    fmap f (Succ a s) = Succ (f a) s
    fmap _ (Soft e s) = Soft e s
    fmap _ (Hard e)   = Hard e

type ParserState = (String, TextLocation) -- Custom user state

newtype Parser a = P (ParserState -> ParserResult a)
newtype ErrorMessage = Err String

parse :: Parser a -> ParserState -> ParserResult a
parse (P p) = p

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (fmap f . parse p)

instance Applicative Parser where
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = P (\state ->
        case parse pf state of
            Succ resf statef ->
                case parse pa statef of
                    Succ resa statea -> Succ (resf resa) statea
                    Soft err statea  -> Soft err statea
                    Hard err         -> Hard err
            Soft errf statef ->
                case parse pa statef of
                    Succ _ statea    -> Soft errf statef
                    Soft erra statea -> Soft (errf ++ erra) statea
                    Hard erra        -> Hard (errf ++ erra)
            Hard errf ->
                Hard errf)

    -- pure :: a -> Parser a
    pure x = P (Succ x)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= fpb = P (\state -> case parse pa state of
        Succ resa statea -> parse (fpb resa) statea
        Soft erra statea -> Soft erra statea
        Hard erra        -> Hard erra)

    -- (>>) :: Parser a -> Parser b -> Parser b
    {-
        This is manually defined because we can do better than the automatic implementation, as unlike in >>=
        the second monad doesn't depend on the result of the first, meaning we can allow for soft errors to
        continue parsing even though they don't require a result.
    -}
    pa >> pb = P (\state -> case parse pa state of
        Succ resa statea -> parse pb statea
        Soft erra statea -> case parse pb statea of
            Succ resb stateb -> Soft erra stateb
            Soft errb stateb -> Soft (erra ++ errb) stateb
            Hard errb        -> Hard (erra ++ errb)
        Hard erra -> Hard erra)

instance MonadError ErrorMessage Parser where
    -- throwError :: e -> m a
    throwError = hard

    -- catchError :: Parser a -> (String -> Parser a) -> Parser a
    catchError pa catch = P (\state -> case parse pa state of
        Succ resa statea   -> Succ resa statea
        {-
            TODO: Work out what may be a more useful implementation of this. Things to think about:
            * How to handle if multiple errors occur, currently all but the first aren't handled
            * Should the catch of a soft parse fail use the state before or after the soft fail?
              Should soft failures even be caught?
        -}
        Soft ((err,_):_) _ -> parse (catch err) state
        Hard ((err,_):_)   -> parse (catch err) state)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P $ const $ Hard []

    -- (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = P (\state -> case parse pa state of
        Succ xa statea -> Succ xa statea
        Soft errs statea -> case parse pb state of
            Succ xb stateb -> Succ xb stateb
            _              -> Soft errs statea
        Hard errs -> case parse pb state of
            Hard _ -> Hard errs
            x      -> x)

hard :: ErrorMessage -> Parser a
hard e = P (\(_, pos) -> Hard [(e, pos)])

soft :: ErrorMessage -> Parser a
soft e = P (\(str, pos) -> Soft [(e, pos)] (str, pos))

peek :: Parser Char
peek = P (\(str, pos) -> case str of
    ""     -> Hard [(Err "Cannot parse past eof", pos)]
    (x:xs) -> Succ x (x:xs,pos))

eof :: Parser Bool
eof = P (\(str, pos) -> case str of
    "" -> Succ True (str, pos)
    _  -> Succ False (str, pos))

next :: Parser ()
next = P (\(str, (line, col)) -> case str of
    "" -> Hard [(Err "Cannot parse past eof", (line, col))]
    (x:xs) -> if x == '\n' then
            Succ () (xs, (line+1, 0))
        else
            Succ () (xs, (line, col+1)))
