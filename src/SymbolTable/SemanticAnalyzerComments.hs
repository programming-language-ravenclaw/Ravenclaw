module SymbolTable.SemanticAnalyzerComments (
    processComments
) where
import AST.AST
import SymbolTable.SymbolTable

processComments :: Literal -> SymbolTable -> SymbolTable
processComments comment@(CommentLiteral (Comment value)) table = 
    let symbolInfo = SymbolInfo "comment" "global" (Just value)
        table' = insertSymbol ("commentLiteral_" ++ value) symbolInfo table
    in table'

