module Parser (
    literalsParser
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

literalsParser :: Parser [Either Literal Comment]
literalsParser = many (try (whiteSpace *> (Left <$> literal <|> Right <$> commentParser) <* whiteSpace)) <* eof
  where
    whiteSpace = skipMany $ oneOf " \t\n"
    
singleLineCommentParser :: Parser Comment
singleLineCommentParser = do
    _ <- string "#"
    comment <- manyTill anyChar (try $ string "\n")
    return $ SingleLineComment comment

multiLineCommentParser :: Parser Comment
multiLineCommentParser = do
    _ <- string "##"
    comment <- manyTill anyChar (try $ lookAhead $ string "##" >> newline)
    _ <- string "##"
    return $ MultiLineComment comment

commentParser :: Parser Comment
commentParser = try singleLineCommentParser <|> try multiLineCommentParser
