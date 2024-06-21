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
expression = try (ArithmeticExpr <$> arithmeticExpression) <|> (BooleanExpr <$> booleanExpression)

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = choice [try floatArithmetic, try intArithmetic, try stringArithmetic, try mixedArithmetic]

intArithmetic :: Parser ArithmeticExpression
intArithmetic = IntArithmetic <$> (IntArith <$> (spaces *> digitParser <* spaces)
                                            <*> (operator <* spaces)
                                            <*> (digitParser <* spaces)
                                            <*> many (try operatorAndDigit))

operatorAndDigit :: Parser OperatorAndDigit
operatorAndDigit = OpAndDigit <$> (spaces *> operator <* spaces) <*> (digitParser <* spaces)

floatArithmetic :: Parser ArithmeticExpression
floatArithmetic = FloatArithmetic <$> (FloatArith <$> (spaces *> floatLiteral <* spaces)
                                                    <*> (operator <* spaces)
                                                    <*> (floatLiteral <* spaces)
                                                    <*> many (try operatorAndFloat))

operatorAndFloat :: Parser OperatorAndFloat
operatorAndFloat = OpAndFloat <$> (spaces *> operator <* spaces) <*> floatLiteral

stringArithmetic :: Parser ArithmeticExpression
stringArithmetic = StringArithmetic <$> (StringArith <$> (spaces *> stringLiteral <* spaces)
                                                      <*> (operatorConcat <* spaces)
                                                      <*> (stringLiteral <* spaces)
                                                      <*> many (try operatorAndString))

operatorAndString :: Parser OperatorAndString
operatorAndString = OpAndString <$> (spaces *> operatorConcat <* spaces) <*> stringLiteral

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
operatorAndMixedInt = OpAndMixedDigit <$> (spaces *> operator <* spaces) <*> digitParser

operatorAndMixedFloat :: Parser OperatorAndMixed
operatorAndMixedFloat = OpAndMixedFloat <$> (spaces *> operator <* spaces) <*> floatLiteral

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

-----

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
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> literal `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']')

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
statement = try (spaces *> (LoopStatement <$> loopStatement
                    <|> ExpressionStatement <$> expression
                    <|> LiteralStatement <$> literal
                    <|> ConditionalStatment <$> conditionalStatementParser
                    <|> Comment <$> comment) <* spaces)

loopStatement :: Parser LoopStatement
loopStatement = try whileLoop <|> try forLoop

statementsParser :: Parser [Statement]
statementsParser = many (try (whitespace *> statement <* whitespace)) <* eof

literalsParser :: Parser [Literal]
literalsParser = many (try (whitespace *> literal <* whitespace)) <* eof

printer :: Parser Printer
printer = Print <$> (reserved "print" *> char '(' *> literal <* char ')')

reserved :: String -> Parser ()
reserved str = () <$ string str <* whiteSpace

whiteSpace :: Parser ()
whiteSpace = skipMany $ oneOf " \t\n"

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