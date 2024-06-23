module Literals.LiteralParser (
    integerLiteral,
    floatLiteral,
    booleanLiteral,
    stringLiteral,
    literal,
    letterParser,
    digitParser,
    identifierPart,
    identifier
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST

literal :: Parser Literal
literal = choice [try (FloatLit <$> floatLiteral), try (IntLit <$> integerLiteral), try (BoolLit <$> booleanLiteral), try (StrLit <$> stringLiteral)]

integerLiteral :: Parser IntegerLiteral
integerLiteral = IntegerLiteral . read <$> many1 digit

floatLiteral :: Parser FloatLiteral
floatLiteral = FloatLiteral . read <$>
                ((\intPart fracPart -> intPart ++ "." ++ fracPart) <$> many1 digit <*> (char '.' *> many1 digit))

booleanLiteral :: Parser BooleanLiteral
booleanLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")

stringLiteral :: Parser StringLiteral
stringLiteral = StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"')

letterParser :: Parser Letter
letterParser = Letter <$> many1 letter

digitParser :: Parser Digit
digitParser = Digit . read <$> many1 digit

identifierPart :: Parser IdentifierPart
identifierPart = (LetterPart <$> letterParser) <|> (DigitPart <$> digitParser)

identifier :: Parser Identifier
identifier = Identifier <$> letterParser <*> many identifierPart