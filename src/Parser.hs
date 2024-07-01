module Parser (
    program
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Statement.StatementParser
import Utils.ParserUtils
import Methods.MethodsParser

-- | The 'globalStatement' parser recognizes and parses a global statement.
--
-- It can parse either a regular statement or a method declaration.
-- 
-- The parser attempts to parse a 'statement' and maps it to the 'Statement' constructor.
-- If that fails, it attempts to parse a 'methodDeclarationParser' and maps it to the 'Method_Declaration' constructor.
globalStatement :: Parser GlobalStatement
globalStatement = Statement <$> statement
                <|> Method_Declaration <$> methodDeclarationParser

-- | The 'program' parser recognizes and parses an entire program.
--
-- It parses zero or more global statements, each surrounded by optional whitespace.
-- The parser ensures that the input ends with 'eof' (end of file).
program :: Parser Program
program = Program <$> many (whitespace *> globalStatement <* whitespace) <* eof