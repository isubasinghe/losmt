{-# LANGUAGE OverloadedStrings #-}

module SAT.Parser where

import Data.Text (Text)
import Data.Void
import qualified SAT.AST as A
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier = takeWhile1P (Just "Identifier") (\s -> s /= '(' && s /= ')' && s /= ' ' && s /= '\n' && s /= '\t')

parseIdentifier :: Parser A.Expr
parseIdentifier = A.Var <$> identifier <?> "<identifier>"

parseNot :: Parser A.Expr
parseNot = parens $ do
  _ <- string "not"
  _ <- space
  e <- parseExpr
  pure $ A.Not e

parseAnd :: Parser A.Expr
parseAnd = parens $ do
  _ <- string "and"
  _ <- space
  lhs <- parseExpr
  _ <- space
  rhs <- parseExpr
  pure $ A.And lhs rhs

parseOr :: Parser A.Expr
parseOr = parens $ do
  _ <- string "or"
  _ <- space
  lhs <- parseExpr
  _ <- space
  rhs <- parseExpr
  pure $ A.Or lhs rhs

parseConst :: Parser A.Expr
parseConst = A.Const <$> (True <$ string "#t" <|> False <$ "#f")

parseExpr :: Parser A.Expr
parseExpr = parseConst <|> parseIdentifier <|> try parseNot <|> try parseAnd <|> parseOr
