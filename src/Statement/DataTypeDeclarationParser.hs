module Statement.DataTypeDeclarationParser (
    -- Parsers and data types Exports
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

-- Parser for data type declarations
-- This parser tries to parse different types of data type declarations
-- (integer, float, boolean, string, list) and returns a DataTypeDeclaration
dataTypeDeclarationParser :: Parser DataTypeDeclaration
dataTypeDeclarationParser = 
    try (DataTypeDeclarationInt <$> parserDataTypeDeclarationInt)
    <|> try (DataTypeDeclarationFloat <$> parserDataTypeDeclarationFloat)
    <|> try (DataTypeDeclarationBool <$> parserDataTypeDeclarationBool)
    <|> try (DataTypeDeclarationString <$> parserDataTypeDeclarationString)
    <|> try (DataTypeDeclarationList <$> parserDataTypeDeclarationList)

-- Parser for integer data type
-- This parser parses the "int" keyword and returns a DataTypeInt
dataTypeIntParser :: Parser DataTypeInt
dataTypeIntParser = 
    DataInt <$> string "int" 
    <?> "int keyword"

-- Parser for float data type
-- This parser parses the "float" keyword and returns a DataTypeFloat
dataTypeFloatParser :: Parser DataTypeFloat
dataTypeFloatParser = 
    DataFloat <$> string "float" 
    <?> "float keyword"

-- Parser for boolean data type
-- This parser parses the "bool" keyword and returns a DataTypeBool
dataTypeBoolParser :: Parser DataTypeBool
dataTypeBoolParser = 
    DataBool <$> string "bool" 
    <?> "bool keyword"

-- Parser for string data type
-- This parser parses the "str" keyword and returns a DataTypeString
dataTypeStringParser :: Parser DataTypeString
dataTypeStringParser = 
    DataString <$> string "str" 
    <?> "str keyword"

-- Parser for list data type
-- This parser parses the "list" keyword and returns a DataTypeList
dataTypeListParser :: Parser DataTypeList
dataTypeListParser = 
    DataList <$> string "list" 
    <?> "list keyword"

-- Parser for data type
-- This parser tries to parse different types of data types
-- (integer, float, boolean, string, list) and returns a DataType
dataType :: Parser DataType
dataType = 
    choice 
        [ IntType <$> dataTypeIntParser
        , FloatType <$> dataTypeFloatParser
        , BoolType <$> dataTypeBoolParser
        , StrType <$> dataTypeStringParser
        , ListType <$> dataTypeListParser
        ]
    -- Expect a data type
    <?> "data type"

-- Parser for integer data type declaration
-- This parser tries to parse an integer data type declaration
-- which can be either an arithmetic integer declaration or an integer literal declaration
parserDataTypeDeclarationInt :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationInt = 
    choice 
        [ try parserDataTypeDeclarationIntArith <?> "an arithmetic int"
        , try parserDataTypeDeclarationIntLit <?> "int literal declaration"
        ]
    <?> "int data type declaration"

-- Parser for arithmetic integer declaration
-- Thisparser parses an integer data type, identifier, and optional arithmetic expression
parserDataTypeDeclarationIntArith :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntArith = 
    DataTypeDecIntArith
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many intArithmetic' <* spaces)
    <?> "int arithmetic declaration"

-- Parser for integer literal declaration
-- This parser parses an integer data type, identifier, and optional integer literals
parserDataTypeDeclarationIntLit :: Parser DataTypeDeclarationInt
parserDataTypeDeclarationIntLit = 
    DataTypeDecIntLit
        <$> (spaces *> dataTypeIntParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many integerLiteral <* spaces)
    <?> "int literal declaration"

-- Parser for float data type declaration
-- This parser tries to parse a float data type declaration
-- which can be either an arithmetic float declaration or a float literal declaration
parserDataTypeDeclarationFloat :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloat = 
    choice 
        [ try parserDataTypeDeclarationFloatArith <?> "an arithmetic float"
        , try parserDataTypeDeclarationFloatLit <?> "float literal declaration"
        ]
    <?> "float data type declaration"

-- Parser for arithmetic float declaration
-- This parser parses a float data type, identifier, and optional arithmetic expression
parserDataTypeDeclarationFloatArith :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatArith = 
    DataTypeDecFloatArith
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many floatArithmetic' <* spaces)
    <?> "float arithmetic declaration"

-- Parser for float literal declaration
-- This parser parses a float data type, identifier, and optional float literals
parserDataTypeDeclarationFloatLit :: Parser DataTypeDeclarationFloat
parserDataTypeDeclarationFloatLit = 
    DataTypeDecFloatLit
        <$> (spaces *> dataTypeFloatParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many floatLiteral <* spaces)
    <?> "float literal declaration"

-- Parser for boolean data type declaration
-- This parser parses a boolean data type, identifier, and optional boolean expression
parserDataTypeDeclarationBool :: Parser DataTypeDeclarationBool
parserDataTypeDeclarationBool = 
    DataTypeDecBool
        <$> (spaces *> dataTypeBoolParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many booleanExpression <* spaces)
    <?> "bool data type declaration"

-- Parser for string data type declaration
-- This parser tries to parse a string data type declaration
-- which can be either an arithmetic string declaration or a string literal declaration
parserDataTypeDeclarationString :: Parser DataTypeDeclarationString
parserDataTypeDeclarationString = 
    choice 
        [ try parserDataTypeDeclarationStringArith
        , try parserDataTypeDeclarationStringLit
        ]
    <?> "string data type declaration"

-- Parser for arithmetic string declaration
-- This parser parses a string data type, identifier, and optional string arithmetic expression
parserDataTypeDeclarationStringArith :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringArith = 
    DataTypeDecStringArith
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many stringArithmetic' <* spaces)
    <?> "string arithmetic declaration"

-- Parser for string literal declaration
-- This parser parses a string data type, identifier, and optional string literals
parserDataTypeDeclarationStringLit :: Parser DataTypeDeclarationString
parserDataTypeDeclarationStringLit = 
    DataTypeDecStringLit
        <$> (spaces *> dataTypeStringParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many stringLiteral <* spaces)
    <?> "string literal declaration"

-- Parser for list data type declaration
-- This parser parses a list data type, identifier, and optional list expression
parserDataTypeDeclarationList :: Parser DataTypeDeclarationList
parserDataTypeDeclarationList = 
    DataTypeDecList
        <$> (spaces *> dataTypeListParser <* spaces)
        <*> (spaces *> identifier <* spaces)
        <*> option [] (spaces *> char '=' *> spaces *> many listExpression <* spaces)
    <?> "list data type declaration"
