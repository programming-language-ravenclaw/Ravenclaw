module Statement.DataTypeDeclarationParser (
    dataTypeDeclarationParser,
    dataTypeIntParser,
    dataTypeFloatParser,
    dataTypeBoolParser,
    dataTypeStringParser,
    dataTypeListParser,
    dataType,
    parserDataTypeDeclarationInt,
    parserDataTypeDeclarationIntArith,
    parserDataTypeDeclarationIntLit,
    parserDataTypeDeclarationFloat,
    parserDataTypeDeclarationFloatArith,
    parserDataTypeDeclarationFloatLit,
    parserDataTypeDeclarationBool,
    parserDataTypeDeclarationString,
    parserDataTypeDeclarationStringArith,
    parserDataTypeDeclarationStringLit,
    parserDataTypeDeclarationList
) where
import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Expression.BooleanExpressionParser
import Expression.ArithmeticExpressionParser
import Literals.LiteralParser
import Expression.ExpressionParser

dataTypeDeclarationParser :: Parser DataTypeDeclaration
dataTypeDeclarationParser = try (DataTypeDeclarationInt <$> parserDataTypeDeclarationInt)
                        <|> try (DataTypeDeclarationFloat <$> parserDataTypeDeclarationFloat)
                        <|> try (DataTypeDeclarationBool <$> parserDataTypeDeclarationBool)
                        <|> try (DataTypeDeclarationString <$> parserDataTypeDeclarationString)
                        <|> try (DataTypeDeclarationList <$> parserDataTypeDeclarationList)

dataTypeIntParser :: Parser DataTypeInt
dataTypeIntParser = DataInt <$> string "int"

dataTypeFloatParser :: Parser DataTypeFloat
dataTypeFloatParser = DataFloat <$> string "float"

dataTypeBoolParser :: Parser DataTypeBool
dataTypeBoolParser = DataBool <$> string "bool"

dataTypeStringParser :: Parser DataTypeString
dataTypeStringParser = DataString <$> string "str"

dataTypeListParser :: Parser DataTypeList
dataTypeListParser = DataList <$> string "list"

dataType :: Parser DataType
dataType = choice
    [ IntType <$> dataTypeIntParser
     , FloatType <$> dataTypeFloatParser
     , BoolType <$> dataTypeBoolParser
     , StrType <$> dataTypeStringParser
     , ListType <$> dataTypeListParser
    ]

parserDataTypeDeclarationInt :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationInt = choice [try parserDataTypeDeclarationIntArith, try parserDataTypeDeclarationIntLit]

parserDataTypeDeclarationIntArith :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntArith = DataTypeDecIntArith
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many intArithmetic' <* spaces)

parserDataTypeDeclarationIntLit :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntLit = DataTypeDecIntLit
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many integerLiteral <* spaces)

parserDataTypeDeclarationFloat :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloat = choice [try parserDataTypeDeclarationFloatArith, try parserDataTypeDeclarationFloatLit]

parserDataTypeDeclarationFloatArith :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatArith = DataTypeDecFloatArith
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many  floatArithmetic' <* spaces)

parserDataTypeDeclarationFloatLit :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatLit = DataTypeDecFloatLit
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many  floatLiteral <* spaces)

parserDataTypeDeclarationBool :: Parser DataTypeDeclarationBool
parserDataTypeDeclarationBool =
    DataTypeDecBool
        <$> (spaces *> dataTypeBoolParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many booleanExpression <* spaces)

parserDataTypeDeclarationString :: Parser DataTypeDeclarationString
parserDataTypeDeclarationString = choice [try parserDataTypeDeclarationStringArith, try parserDataTypeDeclarationStringLit]

parserDataTypeDeclarationStringArith :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringArith = DataTypeDecStringArith
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many  stringArithmetic' <* spaces)

parserDataTypeDeclarationStringLit :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringLit = DataTypeDecStringLit
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many stringLiteral <* spaces)

parserDataTypeDeclarationList :: Parser DataTypeDeclarationList
parserDataTypeDeclarationList  = DataTypeDecList
    <$> (spaces *> dataTypeListParser <* spaces)
    <*> (spaces *> identifier <* spaces)
    <*> option [] (spaces *> char '=' *> spaces *> many listExpression <* spaces)