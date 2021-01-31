{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Layout manipulation
module Lexer.Internal.Layout where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.Fix (MonadFix (mfix))
import Lens.Micro.Platform (each, makeLenses, preuse, use, (%=), (+=), (-=), (.=))
import Lexer.Internal.Error (Error (UnexpectedErr, UnmatchedErr))
import qualified Lexer.Internal.Error as Err
import Lexer.Internal.Position (Col (Col), Coord (Coord), Line (Line), Pos (Pos))
import Lexer.Internal.Token (Tok (CloseBrace, Let, Of, OpenBrace, SemiColon, Where))
import Relude (Bool (False, True), Eq, Int, Monoid (mempty), Ordering (EQ, GT, LT), Show, elem, error, otherwise, (.))
import Relude.Applicative (Applicative)
import Relude.Base (Ord (compare))
import Relude.Function (($), (&), (>>>))
import Relude.Functor (Functor, (<&>))
import Relude.List (drop, head, reverse)
import Relude.Monad (Either (Left, Right), ExceptT, Maybe (Just, Nothing), Monad (return, (>>)), MonadState, State, runExceptT, runState)

data Layout where
  Explicit :: Layout
  Implicit :: Int -> Layout
  deriving (Show, Eq)

data Layout' where
  Layout' ::
    { _layouts :: [] Layout,
      _tokens :: [] Tok,
      _depth :: Int,
      _expectLayout :: Bool
    } ->
    Layout'
  deriving (Show, Eq)

makeLenses ''Layout'

newtype LayoutM a where
  LayoutM :: {_runLayoutM :: ExceptT Error (State Layout') a} -> LayoutM a
  deriving (Functor, Applicative, Monad, MonadState Layout', MonadError Error)

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
run =
  _runLayoutM
    >>> runExceptT
    >>> (`runState` Layout' mempty mempty 0 False)
    >>> \case
      (Left e, _) -> Left e
      (Right _, Layout' _ tkns _ _) -> Right (reverse tkns)

layout :: [] (Pos Tok) -> Either Error ([] Tok)
layout ptks = run (go ptks)
  where
    go :: [] (Pos Tok) -> LayoutM ([] Tok)
    go [] = return []
    go ((Pos ptkn (Coord (Line l, Col c))) : ptkns) =
      if
          | OpenBrace <- ptkn -> do
            startExplicitLayout
            next
          | CloseBrace <- ptkn -> do
            closeExplicitLayout
            next
          | ptkn `elem` [Let, Where, Of] -> do
            expectLayout .= True
            next
          | 0 <- c -> do
            continueImplicitLayout
            next
          | otherwise -> do
            el <- use expectLayout
            when el startImplicitLayout
            next
      where
        next :: LayoutM ([] Tok)
        next = push ptkn >> go ptkns

        closeExplicitLayout :: LayoutM ()
        closeExplicitLayout = do
          depth -= 1
          layout' <- current @Layout
          if
              | Just Explicit <- layout' -> pop @Layout
              | otherwise -> throwError (UnexpectedErr $ Err.String "}")

        startExplicitLayout :: LayoutM ()
        startExplicitLayout = do
          depth += 1
          expectLayout .= False
          push Explicit

        continueImplicitLayout :: LayoutM ()
        continueImplicitLayout = do
          closeFurtherLayouts
          ord <- compareIndent c
          if
              | EQ <- ord -> push SemiColon
              | otherwise -> return ()
          where
            closeFurtherLayouts :: LayoutM ()
            closeFurtherLayouts = do
              ord <- compareIndent c
              if
                  | LT <- ord -> do
                    push CloseBrace
                    pop @Layout
                    closeFurtherLayouts
                  | otherwise -> return ()

        closeImplicitLayouts :: LayoutM ()
        closeImplicitLayouts = do
          layout' <- current @Layout
          if
              | Just Explicit <- layout' -> throwError (UnmatchedErr Err.Layout)
              | Just (Implicit _) <- layout' -> do
                push CloseBrace
                pop @Layout
                closeImplicitLayouts
          return ()

        startImplicitLayout :: LayoutM ()
        startImplicitLayout = do
          expectLayout .= False
          depth += 1
          ord <- compareIndent c
          if
              | GT <- ord -> do
                push OpenBrace
                push (Implicit c)
              | otherwise -> do
                push OpenBrace
                push CloseBrace
                continueImplicitLayout
