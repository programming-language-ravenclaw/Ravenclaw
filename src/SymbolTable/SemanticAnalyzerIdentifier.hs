-- SymbolTable.SemanticAnalyzerIdentifier.hs
module SymbolTable.SemanticAnalyzerIdentifier (
    processIdentifier
) where

import AST.AST
import SymbolTable.SymbolTable

-- | The 'processIdentifier' function takes an 'Identifier' and a 'SymbolTable' as input,
-- and returns an updated 'SymbolTable' with the identifier's information inserted.
processIdentifier :: Identifier -> SymbolTable -> SymbolTable
processIdentifier (Identifier (Letter name) parts) table = 
    let symbolInfo = SymbolInfo "identifier" "global" (Just name)
        table' = insertSymbol name symbolInfo table
        table'' = processIdentifierParts parts table'
    in table''

-- | Helper function to process identifier parts.
processIdentifierParts :: [IdentifierPart] -> SymbolTable -> SymbolTable
processIdentifierParts [] table = table
processIdentifierParts (part:parts) table =
    case part of
        LetterPart (Letter letter) ->
            let table' = insertSymbol letter (SymbolInfo "letter" "global" (Just letter)) table
            in processIdentifierParts parts table'
        DigitPart (Digit digit) ->
            let digitStr = show digit
                table' = insertSymbol digitStr (SymbolInfo "digit" "global" (Just digitStr)) table
            in processIdentifierParts parts table'
