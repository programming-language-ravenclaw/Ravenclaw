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


expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) 
            <|> (BooleanExpr <$> booleanExpression) 
            <|> (LiteralExpr <$> literal) 
            <|> (ListExpression <$> listExpression)
            <|> (MethodCallExpr <$> methodCallParser)

listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)


nameParser :: Parser NameMethod
nameParser = nameMethodParser

argumentParser :: Parser Expression
argumentParser = whitespace *> expression <* whitespace

argumentsParser :: Parser [Expression]
argumentsParser = whitespace *> char '(' *> whitespace *> argumentParser `sepBy` (char ',' *> whitespace) <* char ')' <* whitespace

methodCallParser :: Parser MethodCall
methodCallParser = do
    NameMethod name <- nameParser <?> "method name"
    args <- argumentsParser <?> "method arguments"
    return (MethodCall name args)