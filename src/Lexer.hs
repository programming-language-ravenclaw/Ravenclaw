module Lexer where

{- import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

languageDef :: LanguageDef st
languageDef = emptyDef
    { commentStart = "##",
      commentEnd = "##",
      commentLine = "#",
    }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

whiteSpaces :: Parser ()
whiteSpaces = Text.Parsec.Token.whiteSpace lexer

reservedOps :: String -> Parser ()
reservedOps = Text.Parsec.Token.reservedOp lexer

reservedNa :: String -> Parser ()
reservedNa = Text.Parsec.Token.reserved lexer
 -}
