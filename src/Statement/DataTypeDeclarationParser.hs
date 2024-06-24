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
dataTypeIntParser = DataInt <$> string "int" <?> "int keyword"

dataTypeFloatParser :: Parser DataTypeFloat
dataTypeFloatParser = DataFloat <$> string "float" <?> "float keyword"

dataTypeBoolParser :: Parser DataTypeBool
dataTypeBoolParser = DataBool <$> string "bool" <?> "bool keyword"

dataTypeStringParser :: Parser DataTypeString
dataTypeStringParser = DataString <$> string "str" <?> "str keyword"

dataTypeListParser :: Parser DataTypeList
dataTypeListParser = DataList <$> string "list" <?> "list keyword"

dataType :: Parser DataType
dataType = choice
    [ IntType <$> dataTypeIntParser
     , FloatType <$> dataTypeFloatParser
     , BoolType <$> dataTypeBoolParser
     , StrType <$> dataTypeStringParser
     , ListType <$> dataTypeListParser
    ]
    <?> "data type"

parserDataTypeDeclarationInt :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationInt = choice 
    [ try parserDataTypeDeclarationIntArith <?> "an arithmetic int"
    , try parserDataTypeDeclarationIntLit <?> "int literal declaration"
    ]
    <?> "int data type declaration"

parserDataTypeDeclarationIntArith :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntArith = 
    DataTypeDecIntArith
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many intArithmetic' <* spaces)
    <?> "int arithmetic declaration"

parserDataTypeDeclarationIntLit :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntLit = 
    DataTypeDecIntLit
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many integerLiteral <* spaces)
    <?> "int literal declaration"

parserDataTypeDeclarationFloat :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloat = choice 
    [ try parserDataTypeDeclarationFloatArith <?> "an arithmetic float"
    , try parserDataTypeDeclarationFloatLit <?> "float literal declaration"
    ]
    <?> "float data type declaration"

parserDataTypeDeclarationFloatArith :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatArith = 
    DataTypeDecFloatArith
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many floatArithmetic' <* spaces)
    <?> "float arithmetic declaration"

parserDataTypeDeclarationFloatLit :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatLit = 
    DataTypeDecFloatLit
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many floatLiteral <* spaces)
    <?> "float literal declaration"

parserDataTypeDeclarationBool :: Parser DataTypeDeclarationBool
parserDataTypeDeclarationBool = 
    DataTypeDecBool
        <$> (spaces *> dataTypeBoolParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many booleanExpression <* spaces)
    <?> "bool data type declaration"

parserDataTypeDeclarationString :: Parser DataTypeDeclarationString
parserDataTypeDeclarationString = choice 
    [ try parserDataTypeDeclarationStringArith
    , try parserDataTypeDeclarationStringLit
    ]
    <?> "string data type declaration"

parserDataTypeDeclarationStringArith :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringArith = 
    DataTypeDecStringArith
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*>option [] (spaces *> char '=' *> spaces *> many stringArithmetic' <* spaces)
    <?> "string arithmetic declaration"

parserDataTypeDeclarationStringLit :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringLit = 
    DataTypeDecStringLit
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many stringLiteral <* spaces)
    <?> "string literal declaration"

parserDataTypeDeclarationList :: Parser DataTypeDeclarationList
parserDataTypeDeclarationList = 
    DataTypeDecList
        <$> (spaces *> dataTypeListParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many listExpression <* spaces)

    <?> "list data type declaration"
