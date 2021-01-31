{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Lexer.Lexer (lexer, lexerPos, runLex) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Lens.Micro.Platform (traversed)
import Lexer.Internal.Lexer (Lex, LexT (runLex), char, oneOf, satisfies, string)
import Lexer.Internal.Position (Pos, with)
import Lexer.Internal.Token
  ( Tok
      ( Amp,
        AmpAmp,
        Asterisk,
        Blank,
        Bool,
        BoolTy,
        Case,
        CloseBrace,
        CloseBracket,
        CloseParen,
        Colon,
        ColonColon,
        Comment,
        Data,
        Dollar,
        Else,
        Eq,
        EqEq,
        EqRAngle,
        FSlash,
        FSlashEq,
        If,
        In,
        Int,
        IntTy,
        LAngle,
        LAngleEq,
        Let,
        List,
        Lower,
        Minus,
        NewLine,
        Of,
        OpenBrace,
        OpenBracket,
        OpenParen,
        Plus,
        RAngle,
        RAngleEq,
        SemiColon,
        String,
        StringTy,
        Then,
        Type,
        Underscore,
        Unit,
        Upper,
        Where
      ),
  )
import Relude
  ( Alternative (many, some, (<|>)),
    Applicative ((*>), (<*), (<*>)),
    Bool (False, True),
    Char,
    Eq ((/=), (==)),
    Functor (fmap),
    const,
    error,
    id,
    ($>),
    (&),
    (&&&),
    (.),
    (<$>),
    (<&>),
  )
import Relude.Applicative (Alternative (many, some, (<|>)))
import Relude.Function ((&), (&&&))
import Relude.Functor (($>), (<&>))
import Relude.Unsafe (read)

token :: Lex (Tok, [] Char)
token = white <|> token'
  where
    choices ls = ls <&> fmap string <&> (\(t, l) -> (t,) <$> l) & oneOf

    token' = keyword <|> symbols <|> literal <|> name

    white = blank <|> comment <|> newline
      where
        comment :: Lex (Tok, [] Char)
        comment =
          string "--"
            *> ( satisfies (/= '\n')
                   & many
               )
            <&> (const Comment &&& id)

        newline :: Lex (Tok, [] Char)
        newline =
          ( satisfies (== '\n')
              & some
          )
            <&> (const NewLine &&& id)

        blank :: Lex (Tok, [] Char)
        blank =
          ( satisfies ((== (True, True)) . (isSpace &&& (/= '\n')))
              & some
          )
            <&> (const Blank &&& id)

    keyword :: Lex (Tok, [] Char)
    keyword =
      choices
        [ (Let, "let"),
          (Where, "where"),
          (In, "in"),
          (Data, "data"),
          (Type, "type"),
          (If, "if"),
          (Then, "then"),
          (Else, "else"),
          (Case, "case"),
          (Of, "of"),
          (Underscore, "_")
        ]

    symbols :: Lex (Tok, [] Char)
    symbols =
      choices
        [ (List, "[]"),
          (Unit, "()"),
          (AmpAmp, "&&"),
          (FSlashEq, "/="),
          (ColonColon, "::"),
          (SemiColon, ";"),
          (Colon, ":"),
          (Eq, "="),
          (RAngle, ">"),
          (LAngle, "<"),
          (LAngleEq, "<="),
          (RAngleEq, ">="),
          (EqEq, "=="),
          (EqRAngle, "=>"),
          (OpenParen, "("),
          (CloseParen, ")"),
          (OpenBrace, "{"),
          (CloseBrace, "}"),
          (OpenBracket, "["),
          (CloseBracket, "]"),
          (Plus, "+"),
          (Minus, "-"),
          (FSlash, "/"),
          (Eq, "="),
          (Dollar, "$"),
          (Amp, "&"),
          (Asterisk, "*")
        ]

    literal :: Lex (Tok, [] Char)
    literal = int' <|> string' <|> bool'
      where
        int' :: Lex (Tok, [] Char)
        int' =
          some (satisfies isDigit)
            <&> (Int . read &&& id)

        string' :: Lex (Tok, [] Char)
        string' =
          ( char '"'
              *> many
                (satisfies (/= '"'))
              <* char '"'
          )
            <&> (String &&& id)

        bool' :: Lex (Tok, [] Char)
        bool' =
          choices
            [ (Bool True, "true"),
              (Bool False, "false")
            ]

    name :: Lex (Tok, [] Char)
    name = primTy <|> upperName <|> lowerName
      where
        primTy :: Lex (Tok, [] Char)
        primTy =
          choices
            [ (IntTy, "Int"),
              (StringTy, "String"),
              (BoolTy, "Bool")
            ]
        upperName :: Lex (Tok, [] Char)
        upperName =
          (:) <$> satisfies isUpper <*> many (satisfies isAlphaNum <|> char '\'')
            <&> (Upper &&& id)
        lowerName :: Lex (Tok, [] Char)
        lowerName =
          (:) <$> satisfies isLower <*> many (satisfies isAlphaNum <|> char '\'')
            <&> (Lower &&& id)

lexer :: Lex ([] (Tok, [] Char))
lexer = some token

lexerPos :: Lex ([] (Pos Tok))
lexerPos = lexer <&> with
