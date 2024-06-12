module Parser (
    literalsParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as T
import AST

integerLiteral :: Parser Literal
integerLiteral = IntLiteral . read <$> many1 digit

floatLiteral :: Parser Literal
floatLiteral = do
    intPart <- many1 digit
    _ <- char '.'
    fracPart <- many1 digit
    return $ FloatLiteral (read (intPart ++ "." ++ fracPart))

booleanLiteral :: Parser Literal
booleanLiteral = (BoolLiteral True <$ string "true") <|> (BoolLiteral False <$ string "false")

stringLiteral :: Parser Literal
stringLiteral = do
    _ <- char '"'
    content <- many $ noneOf "\""
    _ <- char '"'
    return $ StringLiteral content

literal :: Parser Literal
literal = choice [try floatLiteral, try integerLiteral, try booleanLiteral, try stringLiteral]

literalsParser :: Parser [Literal]
literalsParser = many (try (whiteSpace *> literal <* whiteSpace)) <* eof
  where
    whiteSpace = skipMany $ oneOf " \t\n"