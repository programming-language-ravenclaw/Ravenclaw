module Expression.ExpressionParser (
    expression,
    listExpression,
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.BooleanExpressionParser
import Expression.ArithmeticExpressionParser

-- | Parser for an expression that can be of various types.
--
--   This function attempts to parse an expression as:
--
--   * An arithmetic expression
--   * A boolean expression
--   * A literal expression
--   * A list of expressions
--   * A method call expression
--
--   Returns: Parsed 'Expression'.
expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression)
          <|>  (BooleanExpr <$> booleanExpression)
          <|>  (LiteralExpr <$> literal)
          <|>  (ListExpression <$> listExpression)

-- | Parser for a list expression enclosed in square brackets.
--
--   Parses expressions separated by commas inside square brackets.
--
--   Returns: Parsed 'ListExpression'.
listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)
