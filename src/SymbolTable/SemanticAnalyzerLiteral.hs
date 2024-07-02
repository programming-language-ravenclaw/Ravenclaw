module SymbolTable.SemanticAnalyzerLiteral (
    processLiteral
) where
import AST.AST
import SymbolTable.SymbolTable

-- | The 'processLiteral' function takes a 'Literal' and a 'SymbolTable' as input,
-- and returns an updated 'SymbolTable' with the literal's information inserted.
--
-- The 'Literal' type represents different types of literals such as integers, floats, booleans,
-- and strings.
-- The 'SymbolTable' is a data structure that holds symbols and their associated information.
--
-- The function works as follows:
--   - For integer literals, it creates a 'SymbolInfo' with type "int" and inserts it into the 
--     table with a key formatted as "intLiteral_<value>".
--   - For float literals, it creates a 'SymbolInfo' with type "float" and inserts it into the 
--     table with a key formatted as "floatLiteral_<value>".
--   - For boolean literals, it creates a 'SymbolInfo' with type "bool" and inserts it into the 
--     table with a key formatted as "boolLiteral_<value>".
--   - For string literals, it creates a 'SymbolInfo' with type "string" and inserts it into the 
--     table with a key formatted as "stringLiteral_<value>".
--
-- Each 'SymbolInfo' contains the type of the literal, the scope (in this case, "global"), and the 
-- literal's value as a string.
processLiteral :: Literal -> SymbolTable -> SymbolTable
processLiteral lit@(IntLit (IntegerLiteral value)) table = 
    let symbolInfo = SymbolInfo "int" "global" (Just (show value))
        table' = insertSymbol ("intLiteral_" ++ show value) symbolInfo table
    in table'

-- | Processes a float literal by inserting its information into the symbol table.
processLiteral lit@(FloatLit (FloatLiteral value)) table = 
    let symbolInfo = SymbolInfo "float" "global" (Just (show value))
        table' = insertSymbol ("floatLiteral_" ++ show value) symbolInfo table
    in table'

-- | Processes a boolean literal by inserting its information into the symbol table.
processLiteral lit@(BoolLit (BooleanLiteral value)) table = 
    let symbolInfo = SymbolInfo "bool" "global" (Just (show value))
        table' = insertSymbol ("boolLiteral_" ++ show value) symbolInfo table
    in table'

-- | Processes a string literal by inserting its information into the symbol table.
processLiteral lit@(StrLit (StringLiteral value)) table = 
    let symbolInfo = SymbolInfo "string" "global" (Just value)
        table' = insertSymbol ("stringLiteral_" ++ value) symbolInfo table
    in table'