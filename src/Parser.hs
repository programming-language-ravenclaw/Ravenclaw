module Parser (
    literalsParser,
    statementsParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as T
import AST
import Data.Char (isLetter, isDigit)

-- Parser de literales
integerLiteral :: Parser Literal
integerLiteral = IntLit . IntegerLiteral . read <$> many1 digit

floatLiteral :: Parser Literal
floatLiteral = do
    intPart <- many1 digit
    _ <- char '.'
    fracPart <- many1 digit
    return $ FloatLit (FloatLiteral (read (intPart ++ "." ++ fracPart)))

booleanLiteral :: Parser Literal
booleanLiteral = (BoolLit (BooleanLiteral True) <$ string "true") <|> (BoolLit (BooleanLiteral False) <$ string "false")

stringLiteral :: Parser Literal
stringLiteral = do
    _ <- char '"'
    content <- many $ noneOf "\""
    _ <- char '"'
    return $ StrLit (StringLiteral content)

literal :: Parser Literal
literal = choice [try floatLiteral, try integerLiteral, try booleanLiteral, try stringLiteral]

-- Parser de operadores
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
    [ Equal <$ string "=="
    , NotEqual <$ string "!="
    , LessThan <$ char '<'
    , GreaterThan <$ char '>'
    , LessThanOrEqual <$ string "<="
    , GreaterThanOrEqual <$ string ">="
    ]

letterParser :: Parser Letter
letterParser = do
    l <- many1 letter
    return $ Letter l

identifierPart :: Parser IdentifierPart
identifierPart = 
    (LetterPart <$> letterPartParser) <|> (DigitPart <$> digitPartParser)
  where
    letterPartParser = Letter <$> many1 letter
    digitPartParser = Digit . read <$> many1 digit

-- Parser de identificadores
identifier :: Parser Identifier
identifier = do
    firstLetter <- letterParser
    parts <- many identifierPart
    return $ Identifier firstLetter parts

-- Parser de expresiones aritméticas

expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) <|> (BooleanExpr <$> booleanExpression)

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = try floatArithmetic <|> intArithmetic <|> stringArithmetic <|> mixedArithmetic

intArithmetic :: Parser ArithmeticExpression
intArithmetic = do
    spaces
    d1 <- Digit . read <$> many1 digit
    spaces
    op <- operator
    spaces
    d2 <- Digit . read <$> many1 digit
    spaces
    rest <- many (try (spaces *> operatorAndDigit <* spaces))
    return $ IntArithmetic (IntArith d1 op d2 rest)

operatorAndDigit :: Parser OperatorAndDigit
operatorAndDigit = do
    spaces
    op <- operator
    spaces
    d <- Digit . read <$> many1 digit
    spaces
    return $ OpAndDigit op d

floatArithmetic :: Parser ArithmeticExpression
floatArithmetic = do
    spaces
    FloatLit f1 <- floatLiteral
    spaces
    op <- operator
    spaces
    FloatLit f2 <- floatLiteral
    spaces
    rest <- many (try (spaces *> operatorAndFloat <* spaces))
    return $ FloatArithmetic (FloatArith f1 op f2 rest)

operatorAndFloat :: Parser OperatorAndFloat
operatorAndFloat = do
    spaces
    op <- operator
    spaces
    FloatLit f <- floatLiteral
    spaces
    return $ OpAndFloat op f

stringArithmetic :: Parser ArithmeticExpression
stringArithmetic = do
    spaces
    StrLit s1 <- stringLiteral
    spaces
    op <- operatorConcat
    spaces
    StrLit s2 <- stringLiteral
    spaces
    rest <- many (try (spaces *> operatorAndString <* spaces))
    return $ StringArithmetic (StringArith s1 op s2 rest)

operatorAndString :: Parser OperatorAndString
operatorAndString = do
    spaces
    op <- operatorConcat
    spaces
    StrLit s <- stringLiteral
    spaces
    return $ OpAndString op s

mixedArithmetic :: Parser ArithmeticExpression
mixedArithmetic = try mixedArithmeticDigit <|> mixedArithmeticFloat

mixedArithmeticDigit :: Parser ArithmeticExpression
mixedArithmeticDigit = do
    spaces
    d <- Digit . read <$> many1 digit
    spaces
    op <- operator
    spaces
    tail <- mixedTail
    spaces
    rest <- many (try (spaces *> operatorAndMixed <* spaces))
    return $ MixedArithmetic (DigitMixed d op tail rest)

mixedArithmeticFloat :: Parser ArithmeticExpression
mixedArithmeticFloat = do
    spaces
    FloatLit f <- floatLiteral
    spaces
    op <- operator
    spaces
    tail <- mixedTail
    spaces
    rest <- many (try (spaces *> operatorAndMixed <* spaces))
    return $ MixedArithmetic (FloatMixed f op tail rest)

mixedTail :: Parser MixedTail
mixedTail = try (spaces *> (DigitTail . Digit . read <$> many1 digit) <* spaces) <|> (spaces *> (FloatTail <$> (unwrapFloatLit <$> floatLiteral)) <* spaces)
  where
    unwrapFloatLit (FloatLit fl) = fl

operatorAndMixed :: Parser OperatorAndMixed
operatorAndMixed = do
    spaces
    op <- operator
    spaces
    tail <- mixedTail
    spaces
    return $ OpAndMixed op tail

-- Parser de expresiones booleanas
booleanExpression :: Parser BooleanExpression
booleanExpression = do
    spaces
    comp <- comparisonExpression
    spaces
    rest <- many (try (spaces *> booleanOpAndComparison <* spaces))
    return $ BooleanExprComparison comp rest

booleanOpAndComparison :: Parser BooleanOpAndComparison
booleanOpAndComparison = do
    spaces
    op <- booleanOperator
    spaces
    comp <- comparisonExpression
    spaces
    return $ BooleanOpComp op comp

comparisonExpression :: Parser ComparisonExpression
comparisonExpression = try literalComparison <|> try arithmeticComparison <|> booleanComparison

literalComparison :: Parser ComparisonExpression
literalComparison = do
    spaces
    lit <- literalExpression
    spaces
    rest <- many (try (spaces *> relationalOpAndLiteral <* spaces))
    return $ LiteralComparison lit rest

arithmeticComparison :: Parser ComparisonExpression
arithmeticComparison = do
    spaces
    arith <- arithmeticExpression
    spaces
    rest <- many1 (try (spaces *> relationalOpAndArith <* spaces))
    return $ ArithmeticComparison arith rest

booleanLiteralParser :: Parser BooleanLiteral
booleanLiteralParser = 
    (BooleanLiteral True <$ string "true") <|> 
    (BooleanLiteral False <$ string "false")

integerLiteralParser :: Parser IntegerLiteral
integerLiteralParser = IntegerLiteral . read <$> many1 digit

floatLiteralParser :: Parser FloatLiteral
floatLiteralParser = do
    intPart <- many1 digit
    _ <- char '.'
    fracPart <- many1 digit
    return $ FloatLiteral (read (intPart ++ "." ++ fracPart))

stringLiteralParser :: Parser StringLiteral
stringLiteralParser = do
    _ <- char '"'
    content <- many (noneOf "\"")
    _ <- char '"'
    return $ StringLiteral content


booleanComparison :: Parser ComparisonExpression
booleanComparison = BooleanComparison <$> booleanLiteralParser

literalExpression :: Parser LiteralExpression
literalExpression = choice [try intExpr, try floatExpr, try strExpr, try boolExpr] -- , try mixedExpr

intExpr :: Parser LiteralExpression
intExpr = do
    spaces
    integerLiteral1 <- integerLiteralParser -- IntegerLiteral . read <$> many1 digit
    spaces
    op <- relationalOperator
    spaces
    integerLiteral2 <- integerLiteralParser -- IntegerLiteral . read <$> many1 digit
    spaces
    rest <- many (try (spaces *> relOpInteger <* spaces))
    return $ IntExpr (IntegerExpression integerLiteral1 op integerLiteral2 rest) -----

relOpInteger :: Parser RelationalOpAndInteger
relOpInteger = do
    spaces
    op <- relationalOperator
    spaces
    integerLiteral <- integerLiteralParser -- IntegerLiteral . read <$> many1 digit
    spaces
    return $ RelOpInteger op integerLiteral

floatExpr :: Parser LiteralExpression
floatExpr = do
    spaces
    floatLiteral1 <- floatLiteralParser
    spaces
    op <- relationalOperator
    spaces
    floatLiteral2 <- floatLiteralParser
    spaces
    rest <- many (try (spaces *> relOpFloat <* spaces))
    return $ FloatExpr (FloatExpression floatLiteral1 op floatLiteral2 rest)

relOpFloat :: Parser RelationalOpAndFloat
relOpFloat = do
    spaces
    op <- relationalOperator
    spaces
    floatLit <- floatLiteralParser
    spaces
    return $ RelOpFloat op floatLit

strExpr :: Parser LiteralExpression
strExpr = do
    spaces
    stringLiteral1 <- stringLiteralParser
    spaces
    op <- relationalOperator
    spaces
    stringLiteral2 <- stringLiteralParser
    spaces
    rest <- many (try (spaces *> relOpString <* spaces))
    return $ StrExpr (StringExpression stringLiteral1 op stringLiteral2 rest)

relOpString :: Parser RelationalOpAndString
relOpString = do
    spaces
    op <- relationalOperator
    spaces
    strLit <- stringLiteralParser
    spaces
    return $ RelOpString op strLit

-----

{- mixedExpr :: Parser LiteralExpression
mixedExpr = do
    spaces
    mixedLiteral <- ml1 -- <- mixedLiteral
    spaces
    op <- relationalOperator
    spaces
    mixedLiteral <-ml2 -- <- mixedLiteral
    spaces
    rest <- many (try (spaces *> relOpMixedLiteral <* spaces))
    return $ MixedExpr (MixedExpression ml1 op ml2 rest)

relOpMixedLiteral :: Parser RelationalOpAndMixedLiteral
relOpMixedLiteral = do
    spaces
    op <- relationalOperator
    spaces
    ml <- mixedLiteral
    spaces
    return $ RelOpMixedLiteral op ml

mixedLiteral :: Parser MixedLiteral -- 
mixedLiteral = try (IntegerLit <$> integerLiteral) <|> (FloatLitMixed <$> floatLiteral) -}

boolExpr :: Parser LiteralExpression
boolExpr = do
    spaces
    b1 <- booleanLiteralParser
    spaces
    op <- relationalOperator
    spaces
    b2 <- booleanLiteralParser
    spaces
    rest <- many (try (spaces *> relOpBoolean <* spaces))
    return $ BoolExpr (BooleanExpressionLiteral b1 op b2 rest)

relOpBoolean :: Parser RelationalOpAndBoolean
relOpBoolean = do
    spaces
    op <- relationalOperator
    spaces
    b <- booleanLiteralParser
    spaces
    return $ RelOpBoolean op b



relationalOpAndLiteral :: Parser RelationalOpAndLiteral
relationalOpAndLiteral = do
    spaces
    op <- relationalOperator
    spaces
    lit <- literalExpression
    spaces
    return $ RelOpLiteral op lit

relationalOpAndArith :: Parser RelationalOpAndArithmetic
relationalOpAndArith = do
    spaces
    op <- relationalOperator
    spaces
    arith <- arithmeticExpression
    spaces
    return $ RelOpArithmetic op arith



-- Parser de bucles
whileLoop :: Parser LoopStatement
whileLoop = do
    _ <- string "while"
    spaces
    _ <- char '('
    spaces
    cond <- booleanExpression
    spaces
    _ <- char ')'
    spaces
    _ <- char '{'
    spaces
    stmts <- many statement
    spaces
    _ <- char '}'
    return $ WhileLoop cond stmts

forLoop :: Parser LoopStatement
forLoop = do
    _ <- string "for"
    spaces
    _ <- char '('
    spaces
    ident <- identifier
    spaces
    _ <- string "in"
    spaces
    lst <- listExpression
    spaces
    _ <- char ')'
    spaces
    _ <- char '{'
    spaces
    stmts <- many statement
    spaces
    _ <- char '}'
    spaces
    return $ ForLoop ident lst stmts

listExpression :: Parser ListExpression
listExpression = do
    spaces
    _ <- char '['
    spaces
    lits <- literal `sepBy` (spaces *> char ',' <* spaces)
    spaces
    _ <- char ']'
    spaces
    return $ ListExpr lits

-- Parser de statements
statement :: Parser Statement
statement = do
    spaces
    stmt <- try (LoopStatement <$> loopStatement) <|> (ExpressionStatement <$> expression)
    spaces
    return stmt

loopStatement :: Parser LoopStatement
loopStatement = whileLoop <|> forLoop

statementsParser :: Parser [Statement]
statementsParser = many (try (whiteSpace *> statement <* whiteSpace)) <* eof
  where
    whiteSpace = skipMany $ oneOf " \t\n"

-- Parser de literales
literalsParser :: Parser [Literal]
literalsParser = many (try (whiteSpace *> literal <* whiteSpace)) <* eof
  where
    whiteSpace = skipMany $ oneOf " \t\n"