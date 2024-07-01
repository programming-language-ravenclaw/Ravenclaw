module Expression.ExpressionParser (
    expression,
    listExpression
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.BooleanExpressionParser
import Expression.ArithmeticExpressionParser

-- | The 'expression' parser recognizes and parses an expression.
--
-- It tries to parse one of the following types of expressions:
--   - Arithmetic expression ('ArithmeticExpr')
--   - Boolean expression ('BooleanExpr')
--   - Literal expression ('LiteralExpr')
--   - List expression ('ListExpression')
expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) 
            <|> (BooleanExpr <$> booleanExpression) 
            <|> (LiteralExpr <$> literal) 
            <|> (ListExpression <$> listExpression)

-- | The 'listExpression' parser recognizes and parses a list expression.
--
-- It parses a list of expressions enclosed in square brackets and separated by commas.
-- The parsed components are used to construct a 'ListExpr' value.
--
-- Example usage:
-- >>> parse listExpression "" "[1, 2, 3]"
-- Right (ListExpr [LiteralExpr (IntLiteral 1), LiteralExpr (IntLiteral 2), LiteralExpr (IntLiteral 3)])
listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)