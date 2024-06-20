    module Parser (
        literalsParser,
        printer,
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

    whiteSpace :: Parser ()
    whiteSpace = skipMany $ oneOf " \t\n"

    literalsParser :: Parser [Literal]
    literalsParser = many (try (whiteSpace *> literal <* whiteSpace)) <* eof


    printer :: Parser Printer
    printer = do
        reserved "print"
        char '('
        lit <- literal
        char ')'
        return (Print lit)

    reserved :: String -> Parser ()
    reserved str = do
        _ <- string str
        whiteSpace

    printerParser :: Parser [Printer]
    printerParser = many (try (whiteSpace *> printer <* whiteSpace)) <* eof