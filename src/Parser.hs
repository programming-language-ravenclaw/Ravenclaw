module Parser (
    literalsParser,
    returnParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as T
import AST

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

literalsParser :: Parser [Literal]
literalsParser = many (try (whiteSpace *> literal <* whiteSpace)) <* eof
  where
    whiteSpace = skipMany $ oneOf " \t\n"


returnStatementParser :: Parser ReturnStatement
-- returnStatementParser = try returnExpr <|> returnLiteral
returnStatementParser = try returnLiteral
  where
    -- returnExpr = do
    --     _ <- string "return"
    --     space
    --     expr <- optionMaybe expression
    --     return $ ReturnStatement expr

    returnLiteral = do
        _ <- string "return"
        space
        lit <- optionMaybe literal
        return $ ReturnStatementLiteral lit


-- expression :: Parser Expression
-- expression = choice
--     [ ArithExpr <$> arithmeticExpression
--     , BoolExpr <$> booleanExpression
--     ]
    
returnParser :: Parser ReturnStatement
returnParser = returnStatementParser <* space

