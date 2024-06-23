module Statement.PrintParser where

import Text.Parsec.Text (Parser)
import AST.AST
import Utils.ParserUtils
import Expression.ExpressionParser

printer :: Parser Printer
printer = Print <$> (reserved "print" *> parens expression)