module Expression.BooleanExpressionParser (
    booleanExpression,
    comparisonExpression,
    arithmeticComparison,
    relationalOpAndArith,
    literalComparison,
    relationalOpAndLiteral,
    booleanComparison,
    booleanOpAndComparison
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Operators.OperatorParser
import Expression.ArithmeticExpressionParser
import Expression.LiteralExpressionParser

booleanExpression :: Parser BooleanExpression
booleanExpression = BooleanExprComparison <$> (spaces *> comparisonExpression <* spaces)
                                          <*> many (try (spaces *> booleanOpAndComparison <* spaces))

booleanOpAndComparison :: Parser BooleanOpAndComparison
booleanOpAndComparison = BooleanOpComp <$> (spaces *> booleanOperator <* spaces)
                                       <*> comparisonExpression

comparisonExpression :: Parser ComparisonExpression
comparisonExpression = choice [try literalComparison, try arithmeticComparison, try booleanComparison]

arithmeticComparison :: Parser ComparisonExpression
arithmeticComparison = ArithmeticComparison <$> (spaces *> arithmeticExpression) <*> many1 (try (spaces *> relationalOpAndArith <* spaces))

relationalOpAndArith :: Parser RelationalOpAndArithmetic
relationalOpAndArith = RelOpArithmetic <$> (spaces *> relationalOperator)
                    <*> (spaces *> arithmeticExpression <* spaces)

literalComparison :: Parser ComparisonExpression
literalComparison = LiteralComparison <$> (spaces *> literalExpression) <*> many (try (spaces *> relationalOpAndLiteral <* spaces))

relationalOpAndLiteral :: Parser RelationalOpAndLiteral
relationalOpAndLiteral = RelOpLiteral <$> (spaces *> relationalOperator)
                            <*> (spaces *> literalExpression <* spaces)

booleanComparison :: Parser ComparisonExpression
booleanComparison = BooleanComparison <$> booleanLiteral
