module Parser (
    program
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Statement.StatementParser
import Utils.ParserUtils

globalStatement :: Parser GlobalStatement
globalStatement = Statement <$> statement

program :: Parser Program
program = Program <$> many (whitespace *> globalStatement <* whitespace) <* eof