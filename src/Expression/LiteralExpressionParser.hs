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

literalExpression :: Parser LiteralExpression
literalExpression = try intExpr 
                    <|> try floatExpr 
                    <|> try strExpr 
                    <|> try boolExpr 
                    <|> try mixedExpr

intExpr :: Parser LiteralExpression
intExpr = IntExpr <$> (spaces *> (IntegerExpression <$> integerLiteral
                                                    <*> (spaces *> relationalOperator <* spaces)
                                                    <*> integerLiteral)
                                                    <*> many (try (spaces *> relOpInteger <* spaces)) <* spaces)

relOpInteger :: Parser RelationalOpAndInteger
relOpInteger = RelOpInteger <$> (spaces *> relationalOperator) <*> (spaces *> integerLiteral <* spaces)

floatExpr :: Parser LiteralExpression
floatExpr = FloatExpr <$> (FloatExpression <$> (spaces *> floatLiteral)
                                   <*> (spaces *> relationalOperator <* spaces)
                                   <*> floatLiteral
                                   <*> many (try (spaces *> relOpFloat <* spaces)))
                                   <* spaces

relOpFloat :: Parser RelationalOpAndFloat
relOpFloat = RelOpFloat <$> (spaces *> relationalOperator) <*> (spaces *> floatLiteral <* spaces)

strExpr :: Parser LiteralExpression
strExpr = StrExpr <$> (StringExpression <$> (spaces *> stringLiteral)
                                  <*> (spaces *> relationalOperator <* spaces)
                                  <*> stringLiteral
                                  <*> many (try (spaces *> relOpString <* spaces)))
                                  <* spaces

relOpString :: Parser RelationalOpAndString
relOpString = RelOpString <$> (spaces *> relationalOperator)
                <*> (spaces *> stringLiteral <* spaces)

boolExpr :: Parser LiteralExpression
boolExpr = BoolExpr <$> (BooleanExpressionLiteral <$> (spaces *> booleanLiteral)
                                           <*> (spaces *> relationalOperator <* spaces)
                                           <*> booleanLiteral
                                           <*> many (try (spaces *> relOpBoolean <* spaces)))
                                           <* spaces

relOpBoolean :: Parser RelationalOpAndBoolean
relOpBoolean = RelOpBoolean <$> (spaces *> relationalOperator)
                 <*> (spaces *> booleanLiteral <* spaces)

mixedExpr :: Parser LiteralExpression
mixedExpr = try mixedExprInt <|> try mixedExprFloat

mixedExprInt :: Parser LiteralExpression
mixedExprInt = MixedExpr <$> (MixedExpressionInteger <$> (spaces *> integerLiteral)
                                       <*> (spaces *> relationalOperator <* spaces)
                                       <*> floatLiteral
                                       <*> many (try (spaces *> relOpAndMixed <* spaces)))
                                       <* spaces

mixedExprFloat :: Parser LiteralExpression
mixedExprFloat = MixedExpr <$> (MixedExpressionFloat <$> (spaces *> floatLiteral)
                                     <*> (spaces *> relationalOperator <* spaces)
                                     <*> integerLiteral
                                     <*> many (try (spaces *> relOpAndMixed <* spaces)))
                                     <* spaces

relOpAndMixed :: Parser RelationalOpAndMixedLiteral
relOpAndMixed = choice [try relOpAndMixedInteger, try relOpAndMixedFloat]

relOpAndMixedInteger :: Parser RelationalOpAndMixedLiteral
relOpAndMixedInteger = RelOpMixedLiteralInteger <$> (spaces *> relationalOperator)
                             <*> (spaces *> integerLiteral)

relOpAndMixedFloat :: Parser RelationalOpAndMixedLiteral
relOpAndMixedFloat = RelOpMixedLiteralFloat <$> (spaces *> relationalOperator)
                           <*> floatLiteral