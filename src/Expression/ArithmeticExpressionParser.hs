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

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = choice [try floatArithmetic, try intArithmetic, try stringArithmetic, try mixedArithmetic]

intArithmetic :: Parser ArithmeticExpression
intArithmetic = IntArithmetic <$> intArithmetic'

intArithmetic' :: Parser IntArithmetic
intArithmetic' = IntArith <$> (spaces *> digitParser <* spaces)
                                            <*> (operator <* spaces)
                                            <*> (digitParser <* spaces)
                                            <*> many (try operatorAndDigit)

operatorAndDigit :: Parser OperatorAndDigit
operatorAndDigit = OpAndDigit <$> (spaces *> operator <* spaces) <*> (digitParser <* spaces)

floatArithmetic :: Parser ArithmeticExpression
floatArithmetic = FloatArithmetic <$> floatArithmetic'

floatArithmetic' :: Parser FloatArithmetic
floatArithmetic' = FloatArith <$> (spaces *> floatLiteral <* spaces)
                              <*> (operator <* spaces)
                              <*> (floatLiteral <* spaces)
                              <*> many (try operatorAndFloat)

operatorAndFloat :: Parser OperatorAndFloat
operatorAndFloat = OpAndFloat <$> (spaces *> operator <* spaces) <*> (floatLiteral <* spaces)

stringArithmetic :: Parser ArithmeticExpression
stringArithmetic = StringArithmetic <$> stringArithmetic'

stringArithmetic' :: Parser StringArithmetic
stringArithmetic' = StringArith <$> (spaces *> stringLiteral <* spaces)
                                <*> (operatorConcat <* spaces)
                                <*> (stringLiteral <* spaces)
                                <*> many (try operatorAndString)

operatorAndString :: Parser OperatorAndString
operatorAndString = OpAndString <$> (spaces *> operatorConcat <* spaces) <*> (stringLiteral <* spaces)

mixedArithmetic :: Parser ArithmeticExpression
mixedArithmetic = try mixedArithmeticDigit <|> try mixedArithmeticFloat

mixedArithmeticDigit :: Parser ArithmeticExpression
mixedArithmeticDigit = MixedArithmetic <$> (DigitMixed <$> (spaces *> digitParser <* spaces)
                                                        <*> (operator <* spaces)
                                                        <*> (floatLiteral <* spaces)
                                                        <*> many (try operatorAndMixed))

mixedArithmeticFloat :: Parser ArithmeticExpression
mixedArithmeticFloat = MixedArithmetic <$> (FloatMixed <$> (spaces *> floatLiteral <* spaces)
                                                        <*> (operator <* spaces)
                                                        <*> (digitParser <* spaces)
                                                        <*> many (try operatorAndMixed))

operatorAndMixed :: Parser OperatorAndMixed
operatorAndMixed = choice [try operatorAndMixedInt, try operatorAndMixedFloat]

operatorAndMixedInt :: Parser OperatorAndMixed
operatorAndMixedInt = OpAndMixedDigit <$> (spaces *> operator <* spaces) <*> (digitParser <* spaces)

operatorAndMixedFloat :: Parser OperatorAndMixed
operatorAndMixedFloat = OpAndMixedFloat <$> (spaces *> operator <* spaces) <*> (floatLiteral <* spaces)
