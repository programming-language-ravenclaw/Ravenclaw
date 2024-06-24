module Methods.NameMethodParser (nameMethodParser) where 

import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser

nameMethodParser :: Parser NameMethod
nameMethodParser = NameMethod <$> identifier