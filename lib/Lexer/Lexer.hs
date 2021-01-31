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
        At,
        Blank,
        Bool,
        BoolTy,
        Case,
        CharTy,
        CloseBrace,
        CloseBracket,
        CloseParen,
        Colon,
        ColonColon,
        Comment,
        Data,
        Dollar,
        Double,
        Else,
        Eq,
        EqEq,
        EqRAngle,
        FSlash,
        FSlashEq,
        Float,
        If,
        In,
        Int32,
        Int32Ty,
        Int64,
        Int64Ty,
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
        Where,
        Word32,
        Word32Ty,
        Word64,
        Word64Ty
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
    (++),
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
          (At, "@"),
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
    literal =
      int32'
        <|> word32'
        <|> int64'
        <|> word64'
        <|> double'
        <|> float'
        <|> string'
        <|> bool'
      where
        double' :: Lex (Tok, [] Char)
        double' =
          ( (:) <$> char '-' <*> double'd
              <|> (char '+' *> double'd)
              <|> double'd
          )
            <&> (Double . read &&& id)
          where
            double'd :: Lex ([] Char)
            double'd = double'' <|> double'' <* (char 'd' <|> char 'D')

            double'' :: Lex ([] Char)
            double'' =
              (\whole dot decim -> whole ++ dot : (decim ++ "0"))
                <$> some (satisfies isDigit)
                <*> char '.'
                <*> many (satisfies isDigit)

        float' :: Lex (Tok, [] Char)
        float' =
          ( (:) <$> char '-' <*> float'd
              <|> (char '+' *> float'd)
              <|> float'd
          )
            <&> (Float . read &&& id)
          where
            float'd :: Lex ([] Char)
            float'd = float'' <|> float'' <* (char 'f' <|> char 'F')

            float'' :: Lex ([] Char)
            float'' =
              (\whole dot decim -> whole ++ dot : (decim ++ "0"))
                <$> some (satisfies isDigit)
                <*> char '.'
                <*> many (satisfies isDigit)

        int32' :: Lex (Tok, [] Char)
        int32' =
          ( (:) <$> char '-' <*> int32''
              <|> char '+' *> int32''
              <|> int32''
          )
            <&> (Int32 . read &&& id)
          where
            int32'' :: Lex ([] Char)
            int32'' = some (satisfies isDigit)

        word32' :: Lex (Tok, [] Char)
        word32' =
          ( char '+' *> word32'u
              <|> word32'u
          )
            <&> (Word32 . read &&& id)
          where
            word32'u :: Lex ([] Char)
            word32'u = word32'' <|> word32'' <* (char 'u' <|> char 'U')

            word32'' :: Lex ([] Char)
            word32'' = some (satisfies isDigit)

        int64' :: Lex (Tok, [] Char)
        int64' =
          ( (:) <$> char '-' <*> int64'l
              <|> char '+' *> int64'l
              <|> int64'l
          )
            <&> (Int64 . read &&& id)
          where
            int64'l :: Lex ([] Char)
            int64'l = int64'' <|> int64'' <* (char 'l' <|> char 'L')

            int64'' :: Lex ([] Char)
            int64'' = some (satisfies isDigit)

        word64' :: Lex (Tok, [] Char)
        word64' =
          ( char '+' *> word64'ul
              <|> word64'ul
          )
            <&> (Word64 . read &&& id)
          where
            word64'ul :: Lex ([] Char)
            word64'ul = word64'' <|> word64'' <* (string "ul" <|> string "UL")

            word64'' :: Lex ([] Char)
            word64'' = some (satisfies isDigit)

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
            [ (Int32Ty, "Int"),
              (Int32Ty, "Int32"),
              (Int32Ty, "I32"),
              (Word32Ty, "Word"),
              (Word32Ty, "Word32"),
              (Word32Ty, "W32"),
              (Int64Ty, "Long"),
              (Int64Ty, "Int64"),
              (Int64Ty, "I64"),
              (Word64Ty, "Word64"),
              (Word64Ty, "W64"),
              (StringTy, "String"),
              (CharTy, "Char"),
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
