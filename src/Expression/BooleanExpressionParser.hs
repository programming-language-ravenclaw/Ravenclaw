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

-- | The 'booleanExpression' parser recognizes and parses a boolean expression.
--
-- It parses a comparison expression followed by zero or more boolean operators and additional comparison expressions.
-- The parsed components are used to construct a 'BooleanExprComparison' value.
--
-- Example usage:
-- >>> parse booleanExpression "" "3 < 4 && 5 > 2"
-- Right (BooleanExprComparison (ArithmeticComparison ...) [(BooleanOpComp And (ArithmeticComparison ...))])
booleanExpression :: Parser BooleanExpression
booleanExpression = BooleanExprComparison <$> (spaces *> comparisonExpression <* spaces)
                                          <*> many (try (spaces *> booleanOpAndComparison <* spaces))

-- | The 'booleanOpAndComparison' parser recognizes and parses a boolean operator followed by a comparison expression.
--
-- It parses a boolean operator, then a comparison expression, and combines them into a 'BooleanOpComp' value.
--
-- Example usage:
-- >>> parse booleanOpAndComparison "" "&& 5 > 2"
-- Right (BooleanOpComp And (ArithmeticComparison ...))
booleanOpAndComparison :: Parser BooleanOpAndComparison
booleanOpAndComparison = BooleanOpComp <$> (spaces *> booleanOperator <* spaces)
                                       <*> comparisonExpression

-- | The 'comparisonExpression' parser recognizes and parses a comparison expression.
--
-- It tries to parse one of the following types of comparison expressions:
--   - Literal comparison ('literalComparison')
--   - Arithmetic comparison ('arithmeticComparison')
--   - Boolean comparison ('booleanComparison')
comparisonExpression :: Parser ComparisonExpression
comparisonExpression = choice [try literalComparison, try arithmeticComparison, try booleanComparison]

-- | The 'arithmeticComparison' parser recognizes and parses an arithmetic comparison expression.
--
-- It parses an arithmetic expression followed by zero or more relational operators and additional arithmetic expressions.
-- The parsed components are used to construct an 'ArithmeticComparison' value.
--
-- Example usage:
-- >>> parse arithmeticComparison "" "3 + 2 < 5 + 8"
-- Right (ArithmeticComparison ...)
arithmeticComparison :: Parser ComparisonExpression
arithmeticComparison = ArithmeticComparison <$> (spaces *> arithmeticExpression) <*> many (try (spaces *> relationalOpAndArith <* spaces))

-- | The 'relationalOpAndArith' parser recognizes and parses a relational operator followed by an arithmetic expression.
--
-- It parses a relational operator, then an arithmetic expression, and combines them into a 'RelOpArithmetic' value.
--
-- Example usage:
-- >>> parse relationalOpAndArith "" "< 5"
-- Right (RelOpArithmetic LessThan (IntArithmetic ...))
relationalOpAndArith :: Parser RelationalOpAndArithmetic
relationalOpAndArith = RelOpArithmetic <$> (spaces *> relationalOperator)
                    <*> (spaces *> arithmeticExpression <* spaces)

-- | The 'literalComparison' parser recognizes and parses a literal comparison expression.
--
-- It parses a literal expression followed by zero or more relational operators and additional literal expressions.
-- The parsed components are used to construct a 'LiteralComparison' value.
--
-- Example usage:
-- >>> parse literalComparison "" "\"hello\" == \"world\""
-- Right (LiteralComparison ...)
literalComparison :: Parser ComparisonExpression
literalComparison = LiteralComparison <$> (spaces *> literalExpression) <*> many (try (spaces *> relationalOpAndLiteral <* spaces))

-- | The 'relationalOpAndLiteral' parser recognizes and parses a relational operator followed by a literal expression.
--
-- It parses a relational operator, then a literal expression, and combines them into a 'RelOpLiteral' value.
--
-- Example usage:
-- >>> parse relationalOpAndLiteral "" "== \"world\""
-- Right (RelOpLiteral Equal (StringLiteral "world"))
relationalOpAndLiteral :: Parser RelationalOpAndLiteral
relationalOpAndLiteral = RelOpLiteral <$> (spaces *> relationalOperator)
                            <*> (spaces *> literalExpression <* spaces)

-- | The 'booleanComparison' parser recognizes and parses a boolean comparison expression.
--
-- It parses a boolean literal and wraps it in the 'BooleanComparison' constructor.
--
-- Example usage:
-- >>> parse booleanComparison "" "true"
-- Right (BooleanComparison (BooleanLiteral True))
booleanComparison :: Parser ComparisonExpression
booleanComparison = BooleanComparison <$> booleanLiteral
