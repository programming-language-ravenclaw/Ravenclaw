module SymbolTable.SemanticAnalyzer.SemanticAnalyzerComments.SemanticAnalyzerComments (
    processComment
) where

import AST.AST
import SymbolTable.SymbolTable

-- | Processes a comment and updates the symbol table.
--
-- This function adds information about comments to the symbol table.
-- For line comments, it adds an entry with the prefix "lineComment_".
-- For block comments, it adds an entry with the prefix "blockComment_".
--
-- Example 1: Line Comment
--
-- >>> let comment = LineComment "This is a line comment"
-- >>> let table = emptyTable
-- >>> processComment comment table
-- insertSymbol ("lineComment_This is a line comment", SymbolInfo "line_comment" "global" (Just "This is a line comment")) emptyTable
--
-- Example 2: Block Comment
--
-- >>> let comment = BlockComment "This is a block comment"
-- >>> let table = emptyTable
-- >>> processComment comment table
-- insertSymbol ("blockComment_This is a block comment", SymbolInfo "block_comment" "global" (Just "This is a block comment")) emptyTable
processComment :: Comment -> SymbolTable -> SymbolTable
processComment (LineComment value) table = 
    let symbolInfo = SymbolInfo "line_comment" (Just value)
        table' = insertSymbol ("lineComment_" ++ value) symbolInfo table
    in table'
processComment (BlockComment value) table = 
    let symbolInfo = SymbolInfo "block_comment" (Just value)
        table' = insertSymbol ("blockComment_" ++ value) symbolInfo table
    in table'
