module Parser (
    literalsParser,
    statementsParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST
import Data.Maybe

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

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
letterParser = do
    l <- many1 letter
    return $ Letter l

identifierPart :: Parser IdentifierPart
identifierPart = 
    (LetterPart <$> letterPartParser) <|> (DigitPart <$> digitPartParser)
  where
    letterPartParser = Letter <$> many1 letter
    digitPartParser = Digit . read <$> many1 digit

identifier :: Parser Identifier
identifier = do
    firstLetter <- letterParser
    parts <- many identifierPart
    return $ Identifier firstLetter parts

expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression) <|> (BooleanExpr <$> booleanExpression)

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = choice [try floatArithmetic, try intArithmetic, try stringArithmetic, try mixedArithmetic]

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
mixedArithmetic = choice [try mixedArithmeticDigit, try mixedArithmeticFloat]

mixedArithmeticDigit :: Parser ArithmeticExpression
mixedArithmeticDigit = do
    spaces
    tail <- Digit . read <$> many1 digit
    spaces
    op <- operator
    spaces
    FloatLit f <- floatLiteral
    spaces
    rest <- many (try (spaces *> operatorAndMixed <* spaces))
    spaces
    return $ MixedArithmetic (DigitMixed tail op f rest)

mixedArithmeticFloat :: Parser ArithmeticExpression
mixedArithmeticFloat = do
    spaces
    FloatLit f <- floatLiteral
    spaces
    op <- operator
    spaces
    tail <- Digit . read <$> many1 digit
    spaces
    rest <- many (try (spaces *> operatorAndMixed <* spaces))
    spaces
    return $ MixedArithmetic (FloatMixed f op tail rest)

operatorAndMixed :: Parser OperatorAndMixed
operatorAndMixed = try operatorAndMixedInt <|> try operatorAndMixedFloat

operatorAndMixedInt :: Parser OperatorAndMixed
operatorAndMixedInt = do
    spaces
    op <- operator
    spaces
    t <- Digit . read <$> many1 digit
    spaces
    return $ OpAndMixedDigit op t

operatorAndMixedFloat :: Parser OperatorAndMixed
operatorAndMixedFloat = do
    spaces
    op <- operator
    spaces
    FloatLit t <- floatLiteral
    spaces
    return $ OpAndMixedFloat op t

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
comparisonExpression = choice [try literalComparison, try arithmeticComparison, try booleanComparison]

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

booleanComparison :: Parser ComparisonExpression
booleanComparison = BooleanComparison <$> booleanLiteralParser

literalExpression :: Parser LiteralExpression
literalExpression = choice [try intExpr, try floatExpr, try strExpr, try boolExpr, try mixedExpr]

intExpr :: Parser LiteralExpression
intExpr = do
    spaces
    IntLit integerLiteral1 <- integerLiteral
    spaces
    op <- relationalOperator
    spaces
    IntLit integerLiteral2 <- integerLiteral
    spaces
    rest <- many (try (spaces *> relOpInteger <* spaces))
    return $ IntExpr (IntegerExpression integerLiteral1 op integerLiteral2 rest)

relOpInteger :: Parser RelationalOpAndInteger
relOpInteger = do
    spaces
    op <- relationalOperator
    spaces
    IntLit intLiteral <- integerLiteral
    spaces
    return $ RelOpInteger op intLiteral

floatExpr :: Parser LiteralExpression
floatExpr = do
    spaces
    FloatLit floatLiteral1 <- floatLiteral
    spaces
    op <- relationalOperator
    spaces
    FloatLit floatLiteral2 <- floatLiteral
    spaces
    rest <- many (try (spaces *> relOpFloat <* spaces))
    return $ FloatExpr (FloatExpression floatLiteral1 op floatLiteral2 rest)

relOpFloat :: Parser RelationalOpAndFloat
relOpFloat = do
    spaces
    op <- relationalOperator
    spaces
    FloatLit floatLit <- floatLiteral
    spaces
    return $ RelOpFloat op floatLit

strExpr :: Parser LiteralExpression
strExpr = do
    spaces
    StrLit stringLiteral1 <- stringLiteral
    spaces
    op <- relationalOperator
    spaces
    StrLit stringLiteral2 <- stringLiteral
    spaces
    rest <- many (try (spaces *> relOpString <* spaces))
    return $ StrExpr (StringExpression stringLiteral1 op stringLiteral2 rest)

relOpString :: Parser RelationalOpAndString
relOpString = do
    spaces
    op <- relationalOperator
    spaces
    StrLit strLit <- stringLiteral
    spaces
    return $ RelOpString op strLit

boolExpr :: Parser LiteralExpression
boolExpr = do
    spaces
    BoolLit b1 <- booleanLiteral
    spaces
    op <- relationalOperator
    spaces
    BoolLit b2 <- booleanLiteral
    spaces
    rest <- many (try (spaces *> relOpBoolean <* spaces))
    return $ BoolExpr (BooleanExpressionLiteral b1 op b2 rest)

relOpBoolean :: Parser RelationalOpAndBoolean
relOpBoolean = do
    spaces
    op <- relationalOperator
    spaces
    BoolLit b <- booleanLiteral
    spaces
    return $ RelOpBoolean op b

mixedExpr :: Parser LiteralExpression
mixedExpr = choice [try mixedExprInt, try mixedExprFloat]

mixedExprInt :: Parser LiteralExpression
mixedExprInt = do
    spaces
    IntLit lit1 <- integerLiteral
    spaces
    op <- relationalOperator
    spaces
    FloatLit f <- floatLiteral
    spaces
    rest <- many (try (spaces *> relOpAndMixed <* spaces))
    return $ MixedExpr (MixedExpressionInteger lit1 op f rest)

mixedExprFloat :: Parser LiteralExpression
mixedExprFloat = do
    spaces
    FloatLit lit1 <- floatLiteral
    spaces
    op <- relationalOperator
    spaces
    IntLit lit2 <- integerLiteral
    spaces
    rest <- many (try (spaces *> relOpAndMixed <* spaces))
    return $ MixedExpr (MixedExpressionFloat lit1 op lit2 rest)

relOpAndMixed :: Parser RelationalOpAndMixedLiteral
relOpAndMixed = choice [try relOpAndMixedInteger, try relOpAndMixedFloat]

relOpAndMixedInteger :: Parser RelationalOpAndMixedLiteral
relOpAndMixedInteger = do
    spaces
    op <- relationalOperator
    spaces
    IntLit lit1 <- integerLiteral
    return $ RelOpMixedLiteralInteger op lit1

relOpAndMixedFloat :: Parser RelationalOpAndMixedLiteral
relOpAndMixedFloat = do
    spaces
    op <- relationalOperator
    FloatLit lit1 <- floatLiteral
    return $ RelOpMixedLiteralFloat op lit1

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

booleanExpressionParser :: Parser ConditionalStatment
booleanExpressionParser = do
  _ <- string "if"
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
  spaces
  diffs <- many diffIfStatementParser
  spaces
  elseStmt <- optionMaybe elseStatementParser
  spaces
  return $ IfStatement cond stmts diffs (maybeToList elseStmt)

-- Parser para la declaración 'diffif'
diffIfStatementParser :: Parser DiffIfStatement
diffIfStatementParser = do
  _ <- string "diffif"
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
  spaces
  return $ DiffIf cond stmts

elseStatementParser :: Parser ElseStatement
elseStatementParser = do
  _ <- string "else"
  spaces
  _ <- char '{'
  spaces
  stmts <- many statement
  spaces
  _ <- char '}'
  spaces
  return $ Else stmts

conditionalStatementParser :: Parser ConditionalStatment
conditionalStatementParser = do
  ifStmt <- booleanExpressionParser
  return ifStmt

statement :: Parser Statement
statement = do
    spaces
    stmt <- try (LoopStatement <$> loopStatement) <|> (ExpressionStatement <$> expression) <|> (LiteralStatement <$> literal) <|> (ConditionalStatment <$> conditionalStatementParser)
    spaces
    return stmt

loopStatement :: Parser LoopStatement
loopStatement = whileLoop <|> forLoop

statementsParser :: Parser [Statement]
statementsParser = many (try (whitespace *> statement <* whitespace)) <* eof

literalsParser :: Parser [Literal]
literalsParser = many (try (whitespace *> literal <* whitespace)) <* eof