{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Lexer internals
module Lexer.Internal.Lexer where

import Control.Arrow (Arrow ((&&&)), (>>>))
import Data.Foldable (Foldable (foldl1, length))
import Data.Function ((&))
import Data.Kind ()
import Data.List (foldl1')
import Lexer.Internal.Error (Error, Unexpected, UnexpectedError (unexpected))
import Relude
  ( Alternative (empty, (<|>)),
    Applicative (pure, (<*>)),
    Bool,
    Char,
    Either (..),
    Eq ((==)),
    Functor,
    Monad (return),
    Ord ((<=)),
    Traversable (traverse),
    ($),
    (&),
    (&&&),
    (.),
    (>>>),
  )
import Relude.Applicative ()

-- | A Lex(er) taek an input, consumes part of it and returns a result + the rest, or fail
newtype LexT c a = Lex {runLex :: [] c -> Either Error ([] c, a)}
  deriving (Functor)

type Lex = LexT Char

instance Applicative Lex where
  pure a = Lex $ pure . (,a)
  Lex f <*> Lex a =
    ( \input -> do
        (input, f) <- f input
        (output, a) <- a input
        return (output, f a)
    )
      & Lex

instance Alternative Lex where
  empty = Lex $ Left . unexpected
  Lex a <|> Lex b =
    ( a &&& b >>> \case
        (Left _, r) -> r
        (r, Left _) -> r
        (a@(Right (oa, _)), b@(Right (ob, _))) ->
          {- the longest match rule wins -}
          if length oa <= length ob
            then a
            else b
    )
      & Lex

satisfies :: (Char -> Bool) -> Lex Char
satisfies p =
  ( \case
      c : cs | p c -> pure (cs, c)
      cs -> Left . unexpected $ cs
  )
    & Lex

char :: Char -> Lex Char
char c = satisfies (c ==)

string :: [] Char -> Lex ([] Char)
string = traverse char

oneOf :: (Foldable t, Alternative f) => t (f a) -> f a
oneOf = foldl1 (<|>)
