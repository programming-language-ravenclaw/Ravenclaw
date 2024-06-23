module Parser (
    program
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST
import Data.Maybe

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

integerLiteral :: Parser IntegerLiteral
integerLiteral = IntegerLiteral . read <$> many1 digit

floatLiteral :: Parser FloatLiteral
floatLiteral = FloatLiteral . read <$>
                ((\intPart fracPart -> intPart ++ "." ++ fracPart) <$> many1 digit <*> (char '.' *> many1 digit))

booleanLiteral :: Parser BooleanLiteral
booleanLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")

stringLiteral :: Parser StringLiteral
stringLiteral = StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"')

literal :: Parser Literal
literal = choice [try (FloatLit <$> floatLiteral), try (IntLit <$> integerLiteral), try (BoolLit <$> booleanLiteral), try (StrLit <$> stringLiteral)]

operator :: Parser Operator
operator = choice
    [ Plus <$ char '+'
    , Minus <$ char '-'
    , Multiply <$ char '*'
    , Divide <$ char '/'
    ]

operatorConcat :: Parser OperatorConcat
operatorConcat = Concat <$ char '+'

booleanOperator :: Parser BooleanOperator
booleanOperator = choice
    [ And <$ string "&&"
    , Or <$ string "||"
    ]

relationalOperator :: Parser RelationalOperator
relationalOperator = choice
    [ Equal <$ try (string "==")
    , NotEqual <$ try (string "!=")
    , LessThan <$ try (char '<' <* notFollowedBy (char '='))
    , GreaterThan <$ try (char '>' <* notFollowedBy (char '='))
    , LessThanOrEqual <$ try (string "<=")
    , GreaterThanOrEqual <$ try (string ">=")
    ]

letterParser :: Parser Letter
letterParser = Letter <$> many1 letter

digitParser :: Parser Digit
digitParser = Digit . read <$> many1 digit

identifierPart :: Parser IdentifierPart
identifierPart = (LetterPart <$> letterParser) <|> (DigitPart <$> digitParser)

identifier :: Parser Identifier
identifier = Identifier <$> letterParser <*> many identifierPart

expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) <|> (BooleanExpr <$> booleanExpression) <|> (LiteralExpr <$> literal) <|> (ListExpression <$> listExpression)

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

booleanExpression :: Parser BooleanExpression
booleanExpression = BooleanExprComparison <$> (spaces *> comparisonExpression <* spaces)
                                          <*> many (try (spaces *> booleanOpAndComparison <* spaces))

booleanOpAndComparison :: Parser BooleanOpAndComparison
booleanOpAndComparison = BooleanOpComp <$> (spaces *> booleanOperator <* spaces)
                                       <*> comparisonExpression

comparisonExpression :: Parser ComparisonExpression
comparisonExpression = choice [try literalComparison, try arithmeticComparison, try booleanComparison]

literalComparison :: Parser ComparisonExpression
literalComparison = LiteralComparison <$> (spaces *> literalExpression) <*> many (try (spaces *> relationalOpAndLiteral <* spaces))

arithmeticComparison :: Parser ComparisonExpression
arithmeticComparison = ArithmeticComparison <$> (spaces *> arithmeticExpression) <*> many1 (try (spaces *> relationalOpAndArith <* spaces))

booleanComparison :: Parser ComparisonExpression
booleanComparison = BooleanComparison <$> booleanLiteral

literalExpression :: Parser LiteralExpression
literalExpression = try intExpr <|> try floatExpr <|> try strExpr <|> try boolExpr <|> try mixedExpr

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

relationalOpAndLiteral :: Parser RelationalOpAndLiteral
relationalOpAndLiteral = RelOpLiteral <$> (spaces *> relationalOperator)
                            <*> (spaces *> literalExpression <* spaces)

relationalOpAndArith :: Parser RelationalOpAndArithmetic
relationalOpAndArith = RelOpArithmetic <$> (spaces *> relationalOperator)
                    <*> (spaces *> arithmeticExpression <* spaces)

whileLoop :: Parser LoopStatement
whileLoop = WhileLoop <$> (string "while" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
              <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

forLoop :: Parser LoopStatement
forLoop = ForLoop <$> (string "for" *> spaces *> char '(' *> spaces *> identifier)
            <*> (spaces *> string "in" *> spaces *> listExpression <* spaces <* char ')')
            <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)

booleanExpressionParser :: Parser ConditionalStatment
booleanExpressionParser = IfStatement
    <$> (string "if" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
    <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
    <*> (spaces *> many diffIfStatementParser)
    <*> (spaces *> (maybeToList <$> optionMaybe elseStatementParser))

diffIfStatementParser :: Parser DiffIfStatement
diffIfStatementParser = DiffIf <$> (string "diffif" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
           <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

elseStatementParser :: Parser ElseStatement
elseStatementParser = Else <$> (string "else" *> spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

conditionalStatementParser :: Parser ConditionalStatment
conditionalStatementParser = booleanExpressionParser

statement :: Parser Statement
statement = choice 
    [ try (spaces *> (LoopStatement <$> loopStatement) <* spaces)
    , try (spaces *> (ConditionalStatement <$> conditionalStatementParser) <* spaces)
    , try (spaces *> (DataTypeDeclarationStatement <$> dataTypeDeclarationParser) <* spaces)
    , try (spaces *> (ExpressionStatement <$> expression) <* spaces)
    , try (spaces *> (LiteralStatement <$> literal) <* spaces)
    , try (spaces *> (Printer <$> printer) <* spaces)
    , try (spaces *> (Comment <$> comment) <* spaces)
    , try (spaces *> (ListStatement <$> listExpression) <* spaces)
    ]

loopStatement :: Parser LoopStatement
loopStatement = try whileLoop <|> try forLoop

globalStatement :: Parser GlobalStatement
globalStatement = Statement <$> statement

program :: Parser Program
program = Program <$> many (whitespace *> globalStatement <* whitespace) <* eof

lineComment :: Parser Comment
lineComment = LineComment <$> (try (string "#" <* notFollowedBy (char '#')) *> manyTill anyChar newline)

blockComment :: Parser Comment
blockComment = BlockComment <$> (string "##" *> manyTill anyChar (try (string "##")))

comment :: Parser Comment
comment = try lineComment <|> try blockComment

reserved :: String -> Parser ()
reserved keyword = try (string keyword *> notFollowedBy alphaNum)

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

printer :: Parser Printer
printer = Print <$> (reserved "print" *> parens expression)

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

data_type_list :: Parser DataTypeList
data_type_list = DataList <$> string "list"

data_type_declaration_list :: Parser DataTypeDeclarationList
data_type_declaration_list  = DataTypeDecList
    <$> (spaces *> data_type_list <* spaces)
    <*> (spaces *> identifier <* spaces)
    <*> (option [] (spaces *> char '=' *> spaces *> many listExpression <* spaces))

dataTypeDeclarationParser :: Parser DataTypeDeclaration
dataTypeDeclarationParser = try (DataTypeDeclarationInt <$> parserDataTypeDeclarationInt)
                        <|> try (DataTypeDeclarationFloat <$> parserDataTypeDeclarationFloat)
                        <|> try (DataTypeDeclarationBool <$> parserDataTypeDeclarationBool)
                        <|> try (DataTypeDeclarationString <$> parserDataTypeDeclarationString)
                        <|> try (DataTypeDeclarationList <$> data_type_declaration_list)

