{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Layout manipulation
module Lexer.Internal.Layout where

import Lens.Micro.Platform (each, makeLenses, preuse, use, (%=))
import Lexer.Internal.Error (Error)
import Lexer.Internal.Position (Pos)
import Lexer.Internal.Token (Tok)
import Relude (Bool, Eq, Int, Ordering (GT), Show, error, (.))
import Relude.Applicative (Applicative)
import Relude.Base (Ord (compare))
import Relude.Functor (Functor, (<&>))
import Relude.List (drop, head)
import Relude.Monad (Either, ExceptT, Maybe (Just, Nothing), Monad, MonadState, State)

data Layout where
  Explicit :: Layout
  Implicit :: Int -> Layout
  deriving (Show, Eq)

data Layout' where
  Layout' ::
    { _layouts :: [] Layout,
      _tokens :: [] Tok,
      _expectingLayout :: Bool
    } ->
    Layout'
  deriving (Show, Eq)

makeLenses ''Layout'

newtype LayoutM a where
  LayoutM :: {_runLayoutM :: ExceptT Error (State Layout') a} -> LayoutM a
  deriving (Functor, Applicative, Monad, MonadState Layout')

makeLenses ''LayoutM

-- | Stack features
class Stack a where
  push :: a -> LayoutM ()
  pop :: LayoutM ()

instance Stack Tok where
  push tok = tokens %= (tok :)
  pop = tokens %= drop 1

instance Stack Layout where
  push lay = layouts %= (lay :)
  pop = layouts %= drop 1

-- | Get the current `Layout` or `Token` if it exists
class Current a where
  current :: LayoutM (Maybe a)

instance Current Layout where
  current = preuse (layouts . each)

instance Current Tok where
  current = preuse (tokens . each)

compareIndent :: Int -> LayoutM Ordering
compareIndent col =
  current <&> \case
    Just Explicit -> GT
    Just (Implicit col') -> compare col col'
    _ -> GT

run :: LayoutM a -> Either Error ([] Tok)
run = error "nyi"

layout :: [] (Pos Tok) -> Either Error ([] Tok)
layout = error "nyi"
