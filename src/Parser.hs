module Parser (
    literalsParser,
    comment
    ) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as T
import Text.Parsec.Combinator
import Text.Parsec.Language
import Control.Monad (void)

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

literalsParser :: Parser [Statement]
literalsParser = many (try (whiteSpace *> (Literal <$> literal) <* whiteSpace <|> Comment <$> comment <* whiteSpace)) <* eof

whiteSpace :: Parser ()
whiteSpace = skipMany $ void $ oneOf " \t\r\n"

lineComment :: Parser Comment
lineComment = LineComment <$> (string "#" *> manyTill anyChar newline)

blockComment :: Parser Comment
blockComment = BlockComment <$> (string "##" *> manyTill anyChar (try (string "##")))

comment :: Parser Comment
comment = try lineComment <|> try blockComment