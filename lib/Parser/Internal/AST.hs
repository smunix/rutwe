-- | Abstract Syntax Tree
module Parser.Internal.AST where

-- | `Rutwe` syntax
newtype AST where
  AST :: {definitions :: [] Definition} -> AST
  deriving (Show, Eq)
