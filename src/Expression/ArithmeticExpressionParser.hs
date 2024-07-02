module Expression.ArithmeticExpressionParser (
    arithmeticExpression,
    intArithmetic,
    intArithmetic',
    operatorAndDigit,
    floatArithmetic,
    floatArithmetic',
    operatorAndFloat,
    stringArithmetic,
    stringArithmetic',
    operatorAndString,
    mixedArithmetic,
    mixedArithmeticDigit,
    mixedArithmeticFloat,
    operatorAndMixed,
    operatorAndMixedInt,
    operatorAndMixedFloat
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Operators.OperatorParser

-- | The 'arithmeticExpression' parser recognizes and parses an arithmetic expression.
--
-- It tries to parse one of the following types of arithmetic expressions:
--   - Floating-point arithmetic ('floatArithmetic')
--   - Integer arithmetic ('intArithmetic')
--   - String concatenation arithmetic ('stringArithmetic')
--   - Mixed arithmetic (both integer and floating-point operands) ('mixedArithmetic')
arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = choice [try floatArithmetic, try intArithmetic, try stringArithmetic, try mixedArithmetic]

-- | The 'intArithmetic' parser recognizes and parses an integer arithmetic expression.
--
-- It parses an integer arithmetic expression using the 'intArithmetic'' parser
-- and wraps the result in the 'IntArithmetic' constructor.
--
-- Example usage:
-- >>> parse intArithmetic "" "3 + 4"
intArithmetic :: Parser ArithmeticExpression
intArithmetic = IntArithmetic <$> intArithmetic'

-- | The 'intArithmetic'' parser recognizes and parses the components of an integer arithmetic expression.
--
-- It parses an integer, an operator, another integer, and zero or more additional operator-and-integer pairs.
-- The parsed components are used to construct an 'IntArith' value.
--
-- Example usage:
-- >>> parse intArithmetic' "" "3 + 4"
-- Right (IntArith 3 Plus 4 [])
intArithmetic' :: Parser IntArithmetic
intArithmetic' = IntArith <$> (spaces *> digitParser <* spaces)
                                            <*> (operator <* spaces)
                                            <*> (digitParser <* spaces)
                                            <*> many (try operatorAndDigit)

-- | The 'operatorAndDigit' parser recognizes and parses an operator followed by an integer.
--
-- It parses an operator, then an integer, and combines them into an 'OpAndDigit' value.
--
-- Example usage:
-- >>> parse operatorAndDigit "" "+ 4"
-- Right (OpAndDigit Plus 4)
operatorAndDigit :: Parser OperatorAndDigit
operatorAndDigit = OpAndDigit <$> (spaces *> operator <* spaces) <*> (digitParser <* spaces)

-- | The 'floatArithmetic' parser recognizes and parses a floating-point arithmetic expression.
--
-- It parses a floating-point arithmetic expression using the 'floatArithmetic'' parser
-- and wraps the result in the 'FloatArithmetic' constructor.
--
-- Example usage:
-- >>> parse floatArithmetic "" "3.5 + 4.2"
-- Right (FloatArithmetic ...)
floatArithmetic :: Parser ArithmeticExpression
floatArithmetic = FloatArithmetic <$> floatArithmetic'

-- | The 'floatArithmetic'' parser recognizes and parses the components of a floating-point arithmetic expression.
--
-- It parses a floating-point number, an operator, another floating-point number, and zero or more additional operator-and-float pairs.
-- The parsed components are used to construct a 'FloatArith' value.
--
-- Example usage:
-- >>> parse floatArithmetic' "" "3.5 + 4.2"
-- Right (FloatArith 3.5 Plus 4.2 [])
floatArithmetic' :: Parser FloatArithmetic
floatArithmetic' = FloatArith <$> (spaces *> floatLiteral <* spaces)
                              <*> (operator <* spaces)
                              <*> (floatLiteral <* spaces)
                              <*> many (try operatorAndFloat)

-- | The 'operatorAndFloat' parser recognizes and parses an operator followed by a floating-point number.
--
-- It parses an operator, then a floating-point number, and combines them into an 'OpAndFloat' value.
--
-- Example usage:
-- >>> parse operatorAndFloat "" "+ 4.2"
-- Right (OpAndFloat Plus 4.2)
operatorAndFloat :: Parser OperatorAndFloat
operatorAndFloat = OpAndFloat <$> (spaces *> operator <* spaces) <*> (floatLiteral <* spaces)

-- | The 'stringArithmetic' parser recognizes and parses a string concatenation expression.
--
-- It parses a string concatenation expression using the 'stringArithmetic'' parser
-- and wraps the result in the 'StringArithmetic' constructor.
--
-- Example usage:
-- >>> parse stringArithmetic "" "\"hello\" + \"world\""
-- Right (StringArithmetic ...)
stringArithmetic :: Parser ArithmeticExpression
stringArithmetic = StringArithmetic <$> stringArithmetic'

-- | The 'stringArithmetic'' parser recognizes and parses the components of a string concatenation expression.
--
-- It parses a string, a concatenation operator, another string, and zero or more additional operator-and-string pairs.
-- The parsed components are used to construct a 'StringArith' value.
--
-- Example usage:
-- >>> parse stringArithmetic' "" "\"hello\" + \"world\""
-- Right (StringArith "hello" Concat "world" [])
stringArithmetic' :: Parser StringArithmetic
stringArithmetic' = StringArith <$> (spaces *> stringLiteral <* spaces)
                                <*> (operatorConcat <* spaces)
                                <*> (stringLiteral <* spaces)
                                <*> many (try operatorAndString)

-- | The 'operatorAndString' parser recognizes and parses a concatenation operator followed by a string.
--
-- It parses a concatenation operator, then a string, and combines them into an 'OpAndString' value.
--
-- Example usage:
-- >>> parse operatorAndString "" "+ \"world\""
-- Right (OpAndString Concat "world")
operatorAndString :: Parser OperatorAndString
operatorAndString = OpAndString <$> (spaces *> operatorConcat <* spaces) <*> (stringLiteral <* spaces)

-- | The 'mixedArithmetic' parser recognizes and parses a mixed arithmetic expression.
--
-- It tries to parse a mixed arithmetic expression with an integer followed by a floating-point number
-- or a mixed arithmetic expression with a floating-point number followed by an integer.
mixedArithmetic :: Parser ArithmeticExpression
mixedArithmetic = try mixedArithmeticDigit <|> try mixedArithmeticFloat

-- | The 'mixedArithmeticDigit' parser recognizes and parses a mixed arithmetic expression starting with an integer.
--
-- It parses an integer, an operator, a floating-point number, and zero or more additional operator-and-mixed pairs.
-- The parsed components are used to construct a 'DigitMixed' value, which is wrapped in the 'MixedArithmetic' constructor.
--
-- Example usage:
-- >>> parse mixedArithmeticDigit "" "3 + 4.2"
-- Right (MixedArithmetic (DigitMixed 3 Plus 4.2 []))
mixedArithmeticDigit :: Parser ArithmeticExpression
mixedArithmeticDigit = MixedArithmetic <$> (DigitMixed <$> (spaces *> digitParser <* spaces)
                                                        <*> (operator <* spaces)
                                                        <*> (floatLiteral <* spaces)
                                                        <*> many (try operatorAndMixed))

-- | The 'mixedArithmeticFloat' parser recognizes and parses a mixed arithmetic expression starting with a floating-point number.
--
-- It parses a floating-point number, an operator, an integer, and zero or more additional operator-and-mixed pairs.
-- The parsed components are used to construct a 'FloatMixed' value, which is wrapped in the 'MixedArithmetic' constructor.
--
-- Example usage:
-- >>> parse mixedArithmeticFloat "" "3.5 + 4"
-- Right (MixedArithmetic (FloatMixed 3.5 Plus 4 []))
mixedArithmeticFloat :: Parser ArithmeticExpression
mixedArithmeticFloat = MixedArithmetic <$> (FloatMixed <$> (spaces *> floatLiteral <* spaces)
                                                        <*> (operator <* spaces)
                                                        <*> (digitParser <* spaces)
                                                        <*> many (try operatorAndMixed))

-- | The 'operatorAndMixed' parser recognizes and parses an operator followed by either an integer or a floating-point number.
--
-- It tries to parse an operator followed by an integer ('operatorAndMixedInt')
-- or an operator followed by a floating-point number ('operatorAndMixedFloat').
operatorAndMixed :: Parser OperatorAndMixed
operatorAndMixed = choice [try operatorAndMixedInt, try operatorAndMixedFloat]

-- | The 'operatorAndMixedInt' parser recognizes and parses an operator followed by an integer.
--
-- It parses an operator, then an integer, and combines them into an 'OpAndMixedDigit' value.
--
-- Example usage:
-- >>> parse operatorAndMixedInt "" "+ 4"
-- Right (OpAndMixedDigit Plus 4)
operatorAndMixedInt :: Parser OperatorAndMixed
operatorAndMixedInt = OpAndMixedDigit <$> (spaces *> operator <* spaces) <*> (digitParser <* spaces)

-- | The 'operatorAndMixedFloat' parser recognizes and parses an operator followed by a floating-point number.
--
-- It parses an operator, then a floating-point number, and combines them into an 'OpAndMixedFloat' value.
--
-- Example usage:
-- >>> parse operatorAndMixedFloat "" "+ 4.2"
-- Right (OpAndMixedFloat Plus 4.2)
operatorAndMixedFloat :: Parser OperatorAndMixed
operatorAndMixedFloat = OpAndMixedFloat <$> (spaces *> operator <* spaces) <*> (floatLiteral <* spaces)
