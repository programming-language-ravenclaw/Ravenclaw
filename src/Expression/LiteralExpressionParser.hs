module Expression.LiteralExpressionParser (
    literalExpression,
    intExpr,
    relOpInteger,
    floatExpr,
    relOpFloat,
    strExpr,
    relOpString,
    boolExpr,
    relOpBoolean,
    mixedExpr,
    mixedExprInt,
    mixedExprFloat,
    relOpAndMixed,
    relOpAndMixedInteger,
    relOpAndMixedFloat
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Operators.OperatorParser

-- | The 'literalExpression' parser recognizes and parses a literal expression.
--
-- It tries to parse one of the following types of literal expressions:
--   - Integer expression ('intExpr')
--   - Float expression ('floatExpr')
--   - String expression ('strExpr')
--   - Boolean expression ('boolExpr')
--   - Mixed literal expression ('mixedExpr')
literalExpression :: Parser LiteralExpression
literalExpression = try intExpr 
                    <|> try floatExpr 
                    <|> try strExpr 
                    <|> try boolExpr 
                    <|> try mixedExpr

-- | The 'intExpr' parser recognizes and parses an integer expression.
--
-- It parses an integer literal followed by a relational operator and another integer literal,
-- and optionally followed by more relational operators and integer literals.
-- The parsed components are used to construct an 'IntExpr' value.
--
-- Example usage:
-- >>> parse intExpr "" "5 < 10"
-- Right (IntExpr (IntegerExpression 5 LessThan 10) [])
intExpr :: Parser LiteralExpression
intExpr = IntExpr <$> (spaces *> (IntegerExpression <$> integerLiteral
                                                    <*> (spaces *> relationalOperator <* spaces)
                                                    <*> integerLiteral)
                                                    <*> many (try (spaces *> relOpInteger <* spaces)) <* spaces)

-- | The 'relOpInteger' parser recognizes and parses a relational operator followed by an integer literal.
--
-- It parses a relational operator, then an integer literal, and combines them into a 'RelOpInteger' value.
--
-- Example usage:
-- >>> parse relOpInteger "" "< 10"
-- Right (RelOpInteger LessThan 10)
relOpInteger :: Parser RelationalOpAndInteger
relOpInteger = RelOpInteger <$> (spaces *> relationalOperator) <*> (spaces *> integerLiteral <* spaces)

-- | The 'floatExpr' parser recognizes and parses a float expression.
--
-- It parses a float literal followed by a relational operator and another float literal,
-- and optionally followed by more relational operators and float literals.
-- The parsed components are used to construct a 'FloatExpr' value.
--
-- Example usage:
-- >>> parse floatExpr "" "5.0 < 10.0"
-- Right (FloatExpr (FloatExpression 5.0 LessThan 10.0) [])
floatExpr :: Parser LiteralExpression
floatExpr = FloatExpr <$> (FloatExpression <$> (spaces *> floatLiteral)
                                   <*> (spaces *> relationalOperator <* spaces)
                                   <*> floatLiteral
                                   <*> many (try (spaces *> relOpFloat <* spaces)))
                                   <* spaces

-- | The 'relOpFloat' parser recognizes and parses a relational operator followed by a float literal.
--
-- It parses a relational operator, then a float literal, and combines them into a 'RelOpFloat' value.
--
-- Example usage:
-- >>> parse relOpFloat "" "< 10.0"
-- Right (RelOpFloat LessThan 10.0)
relOpFloat :: Parser RelationalOpAndFloat
relOpFloat = RelOpFloat <$> (spaces *> relationalOperator) <*> (spaces *> floatLiteral <* spaces)

-- | The 'strExpr' parser recognizes and parses a string expression.
--
-- It parses a string literal followed by a relational operator and another string literal,
-- and optionally followed by more relational operators and string literals.
-- The parsed components are used to construct a 'StrExpr' value.
--
-- Example usage:
-- >>> parse strExpr "" "\"hello\" == \"world\""
-- Right (StrExpr (StringExpression "hello" Equal "world") [])
strExpr :: Parser LiteralExpression
strExpr = StrExpr <$> (StringExpression <$> (spaces *> stringLiteral)
                                  <*> (spaces *> relationalOperator <* spaces)
                                  <*> stringLiteral
                                  <*> many (try (spaces *> relOpString <* spaces)))
                                  <* spaces

-- | The 'relOpString' parser recognizes and parses a relational operator followed by a string literal.
--
-- It parses a relational operator, then a string literal, and combines them into a 'RelOpString' value.
--
-- Example usage:
-- >>> parse relOpString "" "== \"world\""
-- Right (RelOpString Equal "world")
relOpString :: Parser RelationalOpAndString
relOpString = RelOpString <$> (spaces *> relationalOperator)
                <*> (spaces *> stringLiteral <* spaces)

-- | The 'boolExpr' parser recognizes and parses a boolean expression.
--
-- It parses a boolean literal followed by a relational operator and another boolean literal,
-- and optionally followed by more relational operators and boolean literals.
-- The parsed components are used to construct a 'BoolExpr' value.
--
-- Example usage:
-- >>> parse boolExpr "" "true == false"
-- Right (BoolExpr (BooleanExpressionLiteral True Equal False) [])
boolExpr :: Parser LiteralExpression
boolExpr = BoolExpr <$> (BooleanExpressionLiteral <$> (spaces *> booleanLiteral)
                                           <*> (spaces *> relationalOperator <* spaces)
                                           <*> booleanLiteral
                                           <*> many (try (spaces *> relOpBoolean <* spaces)))
                                           <* spaces


-- | The 'relOpBoolean' parser recognizes and parses a relational operator followed by a boolean literal.
--
-- It parses a relational operator, then a boolean literal, and combines them into a 'RelOpBoolean' value.
--
-- Example usage:
-- >>> parse relOpBoolean "" "== false"
-- Right (RelOpBoolean Equal False)
relOpBoolean :: Parser RelationalOpAndBoolean
relOpBoolean = RelOpBoolean <$> (spaces *> relationalOperator)
                 <*> (spaces *> booleanLiteral <* spaces)

-- | The 'mixedExpr' parser recognizes and parses a mixed literal expression.
--
-- It tries to parse one of the following types of mixed literal expressions:
--   - Integer and float mixed expression ('mixedExprInt')
--   - Float and integer mixed expression ('mixedExprFloat')
mixedExpr :: Parser LiteralExpression
mixedExpr = try mixedExprInt <|> try mixedExprFloat

-- | The 'mixedExprInt' parser recognizes and parses a mixed literal expression consisting of an integer and a float.
--
-- It parses an integer literal followed by a relational operator and a float literal,
-- and optionally followed by more relational operators and mixed literals.
-- The parsed components are used to construct a 'MixedExpr' value with an integer base.
--
-- Example usage:
-- >>> parse mixedExprInt "" "5 < 10.0"
-- Right (MixedExpr (MixedExpressionInteger 5 LessThan 10.0) [])
mixedExprInt :: Parser LiteralExpression
mixedExprInt = MixedExpr <$> (MixedExpressionInteger <$> (spaces *> integerLiteral)
                                       <*> (spaces *> relationalOperator <* spaces)
                                       <*> floatLiteral
                                       <*> many (try (spaces *> relOpAndMixed <* spaces)))
                                       <* spaces

-- | The 'mixedExprFloat' parser recognizes and parses a mixed literal expression consisting of a float and an integer.
--
-- It parses a float literal followed by a relational operator and an integer literal,
-- and optionally followed by more relational operators and mixed literals.
-- The parsed components are used to construct a 'MixedExpr' value with a float base.
--
-- Example usage:
-- >>> parse mixedExprFloat "" "5.0 < 10"
-- Right (MixedExpr (MixedExpressionFloat 5.0 LessThan 10) [])
mixedExprFloat :: Parser LiteralExpression
mixedExprFloat = MixedExpr <$> (MixedExpressionFloat <$> (spaces *> floatLiteral)
                                     <*> (spaces *> relationalOperator <* spaces)
                                     <*> integerLiteral
                                     <*> many (try (spaces *> relOpAndMixed <* spaces)))
                                     <* spaces

-- | The 'relOpAndMixed' parser recognizes and parses a relational operator followed by a mixed literal.
--
-- It tries to parse one of the following types of mixed literals:
--   - Relational operator followed by an integer literal ('relOpAndMixedInteger')
--   - Relational operator followed by a float literal ('relOpAndMixedFloat')
relOpAndMixed :: Parser RelationalOpAndMixedLiteral
relOpAndMixed = choice [try relOpAndMixedInteger, try relOpAndMixedFloat]

-- | The 'relOpAndMixedInteger' parser recognizes and parses a relational operator followed by an integer literal.
--
-- It parses a relational operator, then an integer literal, and combines them into a 'RelOpMixedLiteralInteger' value.
--
-- Example usage:
-- >>> parse relOpAndMixedInteger "" "< 10"
-- Right (RelOpMixedLiteralInteger LessThan 10)
relOpAndMixedInteger :: Parser RelationalOpAndMixedLiteral
relOpAndMixedInteger = RelOpMixedLiteralInteger <$> (spaces *> relationalOperator)
                             <*> (spaces *> integerLiteral)

-- | The 'relOpAndMixedFloat' parser recognizes and parses a relational operator followed by a float literal.
--
-- It parses a relational operator, then a float literal, and combines them into a 'RelOpMixedLiteralFloat' value.
--
-- Example usage:
-- >>> parse relOpAndMixedFloat "" "< 10.0"
-- Right (RelOpMixedLiteralFloat LessThan 10.0)
relOpAndMixedFloat :: Parser RelationalOpAndMixedLiteral
relOpAndMixedFloat = RelOpMixedLiteralFloat <$> (spaces *> relationalOperator)
                           <*> floatLiteral