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

-- Parser de identificadores
identifier :: Parser Identifier
identifier = do
    first <- satisfy isLetter
    rest <- many (satisfy (\c -> isLetter c || isDigit c))
    return $ Identifier (Letter first) (map toIdentifierPart rest)
  where
    toIdentifierPart c
      | isLetter c = LetterPart (Letter c)
      | isDigit c  = DigitPart (Digit c)

-- Parser de expresiones aritméticas

expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) <|> (BooleanExpr <$> booleanExpression)


arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = try floatArithmetic <|> intArithmetic <|> stringArithmetic <|> mixedArithmetic

intArithmetic :: Parser ArithmeticExpression
intArithmetic = do
    spaces
    d1 <- Digit <$> digit
    spaces
    op <- operator
    spaces
    d2 <- Digit <$> digit
    spaces
    rest <- many (try (spaces *> operatorAndDigit <* spaces))
    return $ IntArithmetic (IntArith d1 op d2 rest)

operatorAndDigit :: Parser OperatorAndDigit
operatorAndDigit = do
    spaces
    op <- operator
    spaces
    d <- Digit <$> digit
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
    d <- Digit <$> digit
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
mixedTail = try (spaces *> (DigitTail . Digit <$> digit) <* spaces) <|> (spaces *> (FloatTail <$> (unwrapFloatLit <$> floatLiteral)) <* spaces)
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
comparisonExpression = do
    spaces
    arith <- arithmeticExpression
    spaces
    rest <- many (try (spaces *> relationalOpAndArith <* spaces))
    return $ Comparison arith rest

relationalOpAndArith :: Parser RelationalOpAndArith
relationalOpAndArith = do
    spaces
    op <- relationalOperator
    spaces
    arith <- arithmeticExpression
    spaces
    return $ RelationalOpArith op arith

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