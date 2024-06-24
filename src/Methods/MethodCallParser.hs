module Methods.MethodCallParser (
    nameMethodParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST

nameMethodParser :: Parser NameMethod
nameMethodParser = NameMethod <$> identifier
