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



expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) 
            <|> (BooleanExpr <$> booleanExpression) 
            <|> (LiteralExpr <$> literal) 
            <|> (ListExpression <$> listExpression)
            <|> (MethodCallExpr <$> methodCallParser)

listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)

methodCallParser :: Parser MethodCall
methodCallParser = do
    name <- nameMethodParser
    _ <- char '('
    args <- expression `sepBy` (spaces *> char ',' <* spaces)
    _ <- char ')'
    return $ MethodCall name args