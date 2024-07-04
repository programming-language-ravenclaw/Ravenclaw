module Utils.ParserUtils (
    whitespace,
    reserved,
    parens,
    printerReservedWords
) where

import Text.Parsec
import Text.Parsec.Text (Parser)

-- | The 'whitespace' parser skips over any number of whitespace characters.
--
-- It skips characters such as spaces, tabs, and newlines.
whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

-- | The 'reserved' parser recognizes and parses a reserved keyword.
--
-- Takes a 'String' representing the keyword to parse.
-- It ensures that the keyword is not immediately followed by an alphanumeric character,
-- preventing it from matching prefixes of longer identifiers.
--
-- The parser attempts to parse the keyword using 'try', which allows backtracking if the parse fails.
reserved :: String -> Parser ()
reserved keyword = try (string keyword *> notFollowedBy alphaNum)

-- | The 'parens' parser parses an expression surrounded by parentheses.
--
-- Takes a 'Parser' for the expression to be parsed inside the parentheses.
-- It skips any spaces around the expression.
--
-- Returns the result of the inner parser.
parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'


-- | Gives a list of a reserved words declared on BNF. 
printerReservedWords :: [String]
printerReservedWords = [
    "if", "diffif", "else", "while", "for", "in", "method", "return",
    "int", "float", "bool", "str", "list", "true", "false", "print"
    ]