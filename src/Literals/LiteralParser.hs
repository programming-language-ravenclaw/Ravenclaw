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

-- | Parses any kind of literal (integer, float, boolean, or string).
-- Returns a 'Literal' AST node.
literal :: Parser Literal
literal = choice [try (FloatLit <$> floatLiteral), try (IntLit <$> integerLiteral), try (BoolLit <$> booleanLiteral), try (StrLit <$> stringLiteral)]

-- | Parses an integer literal.
-- Returns an 'IntegerLiteral' AST node.
integerLiteral :: Parser IntegerLiteral
integerLiteral = IntegerLiteral . read <$> many1 digit

-- | Parses a floating point literal.
-- Returns a 'FloatLiteral' AST node.
floatLiteral :: Parser FloatLiteral
floatLiteral = FloatLiteral . read <$>
                ((\intPart fracPart -> intPart ++ "." ++ fracPart) <$> many1 digit <*> (char '.' *> many1 digit))

-- | Parses a boolean literal ("true" or "false").
-- Returns a 'BooleanLiteral' AST node.
booleanLiteral :: Parser BooleanLiteral
booleanLiteral = (BooleanLiteral True <$ string "true") <|> (BooleanLiteral False <$ string "false")

-- | Parses a string literal enclosed in double quotes.
-- Returns a 'StringLiteral' AST node.
stringLiteral :: Parser StringLiteral
stringLiteral = StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"')

-- | Parses a sequence of letters.
-- Returns a 'Letter' AST node.
letterParser :: Parser Letter
letterParser = Letter <$> many1 letter

-- | Parses a sequence of digits.
-- Returns a 'Digit' AST node.
digitParser :: Parser Digit
digitParser = Digit . read <$> many1 digit

-- | Parses a part of an identifier, which can be either letters or digits.
-- Returns an 'IdentifierPart' AST node.
identifierPart :: Parser IdentifierPart
identifierPart = (LetterPart <$> letterParser) <|> (DigitPart <$> digitParser)

-- | Parses an identifier starting with a letter followed by any number of identifier parts.
-- Returns an 'Identifier' AST node.
identifier :: Parser Identifier
identifier = Identifier <$> letterParser <*> many identifierPart