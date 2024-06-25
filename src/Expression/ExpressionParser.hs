module Expression.ExpressionParser (
    expression,
    listExpression,
    methodCallParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.BooleanExpressionParser
import Expression.ArithmeticExpressionParser
import Utils.ParserUtils (whitespace)
import Methods.NameMethodParser (nameMethodParser)

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
            <|> (BooleanExpr <$> booleanExpression) 
            <|> (LiteralExpr <$> literal) 
            <|> (ListExpression <$> listExpression)
            <|> (MethodCallExpr <$> methodCallParser)

-- | Parser for a list expression enclosed in square brackets.
--
--   Parses expressions separated by commas inside square brackets.
--
--   Returns: Parsed 'ListExpression'.
listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)

-- | Parser for the name of a method.
--
--   Returns: Parsed 'NameMethod'.
nameParser :: Parser NameMethod
nameParser = nameMethodParser

-- | Parser for an expression argument.
--
--   Returns: Parsed 'Expression'.
argumentParser :: Parser Expression
argumentParser = whitespace *> expression <* whitespace

-- | Parser for a list of arguments separated by commas and enclosed in parentheses.
--
--   Returns: List of parsed 'Expression's.
argumentsParser :: Parser [Expression]
argumentsParser = whitespace *> char '(' *> whitespace *> argumentParser `sepBy` (char ',' *> whitespace) <* char ')' <* whitespace

-- | Parser for a method call.
--
--   Parses a method call consisting of a method name followed by a list of arguments.
--
--   Returns: Parsed 'MethodCall'.
methodCallParser :: Parser MethodCall
methodCallParser = do
    NameMethod name <- nameParser <?> "method name"
    args <- argumentsParser <?> "method arguments"
    return (MethodCall name args)