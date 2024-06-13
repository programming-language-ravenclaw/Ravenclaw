    module Parser (
        literalsParser,
        printParser,
        instructionParser,
        instructionsParser
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

    printParser :: Parser [Literal] 
    printParser = do
        _ <- string "print"
        _  <- char '('
        lit <- literal
        _ <- char ')'
        return [PrintLit lit]

    literal :: Parser Literal
    literal = choice [try floatLiteral, try integerLiteral, try booleanLiteral, try stringLiteral]


    whiteSpace = skipMany $ oneOf " \t\n"

    literalsParser :: Parser [Literal]
    literalsParser = many (try (whiteSpace *> literal <* whiteSpace)) <* eof


    parsers :: Parser [Literal]
    parsers = many (try (whiteSpace *> literal <* whiteSpace)) <* eof


    instructionParser :: Parser Literal
    instructionParser = (try printParser >>= return . head) <|> literal


    instructionsParser :: Parser [Literal]
    instructionsParser = many (try (whiteSpace *> instructionParser <* whiteSpace)) <* eof
