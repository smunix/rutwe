{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Lexer errors
module Lexer.Internal.Error where

import Relude (Char, Eq, Show, String, (.))

-- | Unexpected errors
data Unexpected where
  Char :: Char -> Unexpected
  String :: String -> Unexpected
  Eof :: Unexpected
  deriving (Show, Eq)

-- | The kind of erros that can occur
data Error where
  UnexpectedErr :: Unexpected -> Error
  deriving (Show, Eq)

-- | The class of types that can be made into an Unexpected Error
class UnexpectedError s where
  unexpected :: s -> Error

instance UnexpectedError ([] Char) where
  unexpected = UnexpectedErr . String

instance UnexpectedError Char where
  unexpected = UnexpectedErr . Char

-- instance (Show c) => UnexpectedError ([] c) where
--   unexpected = UnexpectedErr . String . show
