module Parser (
    program
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Statement.StatementParser
import Utils.ParserUtils
import Methods.MethodsParser

globalStatement :: Parser GlobalStatement
globalStatement = Statement <$> statement
                <|> Method_Declaration <$> methodDeclarationParser

program :: Parser Program
program = Program <$> many (whitespace *> globalStatement <* whitespace) <* eof