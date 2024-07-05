module Methods.NameMethodParser (nameMethodParser) where 

import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser

-- | Parser for a method name.
--
--   Parses an 'Identifier' and constructs a 'NameMethod'.
--
--   Returns: Parsed 'NameMethod'.
nameMethodParser :: Parser NameMethod
nameMethodParser = NameMethod <$> identifier