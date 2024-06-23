module Utils.ParserUtils (
    whitespace,
    reserved,
    parens
) where

import Text.Parsec
import Text.Parsec.Text (Parser)

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \t\n"

reserved :: String -> Parser ()
reserved keyword = try (string keyword *> notFollowedBy alphaNum)

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'
