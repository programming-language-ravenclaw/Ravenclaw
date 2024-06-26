module Statement.PrintParser where

import Text.Parsec.Text (Parser)
import AST.AST
import Utils.ParserUtils
import Expression.ExpressionParser

-- StatementParser a 'Printer' statement.
-- The 'printer' function parses the "print" keyword followed by an expression
-- enclosed in parentheses. It returns a 'Printer' value representing the parsed
-- statement.
-- print(123)
-- print("hello world")
printer :: Parser Printer
printer = Print <$> (reserved "print" *> parens expression)