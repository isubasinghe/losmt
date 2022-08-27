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
parseNot = A.Not <$> (parens $ string "not" *> space1 *> parseExpr)

parseAnd :: Parser A.Expr
parseAnd = parens $ do
  lhs <- parseExpr
  _ <- space1
  rhs <- parseExpr
  pure $ A.And lhs rhs

parseOr :: Parser A.Expr 
parseOr = parens $ do 
  lhs <- parseExpr 
  _ <- space1 
  rhs <- parseExpr 
  pure $ A.Or lhs rhs

parseConst :: Parser A.Expr 
parseConst =  A.Const <$> (True <$ string "#t" <|> False <$ "#f")

parseExpr :: Parser A.Expr
parseExpr = parseIdentifier <|> parseConst <|> parseNot <|> parseAnd <|> parseOr
