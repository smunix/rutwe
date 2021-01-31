{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Track token positions in files
module Lexer.Internal.Position where

import Lexer.Internal.Token (Tok (Blank, Comment, NewLine))
import Relude (Char, Eq, Int, Num ((+)), Show, coerce, error)
import Relude.Foldable (Foldable (length))
import Relude.Function (fix, (&))
import Relude.Monad (Maybe (Just, Nothing))

-- | Position in line
newtype Col where
  Col :: {unCol :: Int} -> Col
  deriving (Show, Eq)

newtype Line where
  Line :: {unLine :: Int} -> Line
  deriving (Show, Eq)

newtype Coord where
  Coord :: (Line, Col) -> Coord
  deriving (Show, Eq)

data Pos a where
  Pos :: a -> Coord -> Pos a
  deriving (Show, Eq)

with :: [] (Tok, [] Char) -> [] (Pos Tok)
with = go (Coord (Line 0, Col 0))
  where
    go :: Coord -> [] (Tok, [] Char) -> [] (Pos Tok)
    go = fix \rec !coord !tks ->
      if
          | [] <- tks -> []
          | t : ts <- tks -> case go' t coord of
            (Nothing, coord') -> rec coord' ts
            (Just ptk, coord') -> ptk : rec coord' ts
    go' :: (Tok, [] Char) -> Coord -> (Maybe (Pos Tok), Coord)
    go' !tk coord@(Coord (!l, !c)) =
      if
          | (NewLine, s) <- tk -> (Nothing, Coord (unLine l + length s & Line, Col 0))
          | (Comment, comment) <- tk -> (Nothing, Coord (l, unCol c + length comment & Col))
          | (Blank, blank) <- tk -> (Nothing, Coord (l, unCol c + length blank & Col))
          | (tok, s) <- tk -> (Just (Pos tok coord), Coord (l, unCol c + length s & Col))
