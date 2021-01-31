{-# LANGUAGE NoImplicitPrelude #-}

-- | The kind of tokens we can Lex
module Lexer.Internal.Token where

import Relude (Bool, Double, Eq, Float, Int32, Int64, Show, String, Word32, Word64)

data Tok where
  Let ::
    -- | `let`
    Tok
  Where ::
    -- | `where`
    Tok
  In ::
    -- | `in`
    Tok
  Data ::
    -- | `data`
    Tok
  Type ::
    -- | `type`
    Tok
  If ::
    -- | `if`
    Tok
  Then ::
    -- | `then`
    Tok
  Else ::
    -- | `else`
    Tok
  Case ::
    -- | `case`
    Tok
  Of ::
    -- | `of`
    Tok
  Underscore ::
    -- | `_`
    Tok
  OpenParen ::
    -- | `(`
    Tok
  CloseParen ::
    -- | `)`
    Tok
  Unit ::
    -- | `()`
    Tok
  OpenBrace ::
    -- | `{`
    Tok
  CloseBrace ::
    -- | `}`
    Tok
  OpenBracket ::
    -- | `[`
    Tok
  CloseBracket ::
    -- | `]`
    Tok
  List ::
    -- | `[]`
    Tok
  Colon ::
    -- | `:`
    Tok
  SemiColon ::
    -- | `;`
    Tok
  ColonColon ::
    -- | `::`
    Tok
  MinusRAngle ::
    -- | `->`
    Tok
  EqRAngle ::
    -- | `=>`
    Tok
  Bar ::
    -- | `|`
    Tok
  BSlash ::
    -- | `\`
    Tok
  FSlash ::
    -- | `/`
    Tok
  Plus ::
    -- | `+`
    Tok
  PlusPlus ::
    -- | `++`
    Tok
  Minus ::
    -- | `-`
    Tok
  Asterisk ::
    -- | `*`
    Tok
  Eq ::
    -- | `=`
    Tok
  Dollar ::
    -- | `$`
    Tok
  LAngle ::
    -- | `<`
    Tok
  RAngle ::
    -- | `>`
    Tok
  Dot ::
    -- | `.`
    Tok
  LAngleEq ::
    -- | `<=`
    Tok
  RAngleEq ::
    -- | `>=`
    Tok
  EqEq ::
    -- | `==`
    Tok
  FSlashEq ::
    -- | `/=`
    Tok
  BarBar ::
    -- | `||`
    Tok
  At ::
    -- | `@`
    Tok
  Amp ::
    -- | `&`
    Tok
  AmpAmp ::
    -- | `&&`
    Tok
  Int32 :: Int32 -> Tok
  Int64 :: Int64 -> Tok
  Word32 :: Word32 -> Tok
  Word64 :: Word64 -> Tok
  Double :: Double -> Tok
  Float :: Float -> Tok
  String :: String -> Tok
  Bool :: Bool -> Tok
  CharTy :: Tok
  Int32Ty :: Tok
  Int64Ty :: Tok
  Word32Ty :: Tok
  Word64Ty :: Tok
  FloatTy :: Tok
  DoubleTy :: Tok
  StringTy :: Tok
  BoolTy :: Tok
  Upper :: String -> Tok
  Lower :: String -> Tok
  Blank :: Tok
  Comment :: Tok
  NewLine :: Tok
  deriving (Eq, Show)
