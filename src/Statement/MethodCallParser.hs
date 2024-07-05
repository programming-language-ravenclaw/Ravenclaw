module Statement.MethodCallParser (
    methodCallParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Expression.ExpressionParser (expression)
import Utils.ParserUtils (whitespace, parens)
import Methods.NameMethodParser (nameMethodParser)


-- | Parser for a method call.
--
--   Parses a method call consisting of a method name followed by a list of arguments.
--
--   Returns: Parsed 'MethodCall'.
methodCallParser :: Parser Statement
methodCallParser =
    MethodCallStatement <$> (MethodCall
                             <$> (extractIdentifier <$> (whitespace *> nameMethodParser <* whitespace))
                             <*> argumentsParser)

-- | Parser for a list of arguments separated by commas and enclosed in parentheses.
--
--   Returns: List of parsed 'Expression's.
argumentsParser :: Parser [Expression]
argumentsParser = parens $ whitespace *> argumentParser `sepBy` (char ',' *> whitespace)

-- | Extracts the Identifier from a NameMethod
extractIdentifier :: NameMethod -> Identifier
extractIdentifier (NameMethod ident) = ident

argumentParser :: Parser Expression
argumentParser = whitespace *> expression <* whitespace
