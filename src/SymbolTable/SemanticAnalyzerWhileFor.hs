module SymbolTable.SemanticAnalyzerWhileFor (
    processWhile,
    processFor
) where

import AST.AST
import SymbolTable.SymbolTable

-- | The 'processWhile' function takes a 'While' statement and a 'SymbolTable' as input,
-- and returns an updated 'SymbolTable' with the while statement's information inserted.
--
-- The 'While' type represents a while loop in the AST.
-- The 'SymbolTable' is a data structure that holds symbols and their associated information.
--
-- The function works as follows:
--   - It creates a 'SymbolInfo' with type "while" and inserts it into the table with a key formatted as "while_<line>".
--   - It then recursively processes the while loop's body by calling 'processBlock'.
--
-- Each 'SymbolInfo' contains the type of the while loop, the scope (in this case, "global"), and the
-- line number of the while loop.
processWhile :: LoopStatement -> SymbolTable -> SymbolTable
processWhile (WhileLoop line block) table =
    let symbolInfo = SymbolInfo "while" "global" (Just (show line))
        table' = insertSymbol ("while_" ++ show line) symbolInfo table
        table'' = processBlock block table'
    in table''

-- | The 'processFor' function takes a 'For' statement and a 'SymbolTable' as input,
-- and returns an updated 'SymbolTable' with the for statement's information inserted.
--
-- The 'For' type represents a for loop in the AST.
-- The 'SymbolTable' is a data structure that holds symbols and their associated information.
--
-- The function works as follows:
--   - It creates a 'SymbolInfo' with type "for" and inserts it into the table with a key formatted as "for_<line>".
--   - It then recursively processes the for loop's body by calling 'processBlock'.
--
-- Each 'SymbolInfo' contains the type of the for loop, the scope (in this case, "global"), and the
-- line number of the for loop.
{- processFor :: LoopStatement -> SymbolTable -> SymbolTable
processFor (ForLoop line block) table =
    let symbolInfo = SymbolInfo "for" "global" (Just (show line))
        table' = insertSymbol ("for_" ++ show line) symbolInfo table
        table'' = processBlock block table'
    in table''
 -}
processFor :: LoopStatement -> SymbolTable -> SymbolTable
processFor (ForLoop line initExpr block) table =
    let symbolInfo = SymbolInfo "for" "global" (Just (show line))
        table' = insertSymbol ("for_" ++ show line) symbolInfo table
        -- Process the initialization expression
        table'' = processExpression initExpr table'
        -- Process the block
        table''' = processBlock block table''
    in table'''

-- | Processes a block of statements by recursively calling 'processStatement' on each statement.

processBlock :: [Statement] -> SymbolTable -> SymbolTable
processBlock [] table = table
processBlock (stmt:stmts) table =
    let table' = processStatement stmt table
    in processBlock stmts table'

-- | Processes a statement by calling the appropriate function based on the statement type.

processStatement :: Statement -> SymbolTable -> SymbolTable
processStatement (LoopStatement (WhileLoop line block)) table = processWhile (WhileLoop line block) table