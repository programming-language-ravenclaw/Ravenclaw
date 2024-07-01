module SymbolTable.SemanticAnalyzer where

import AST.AST
import SymbolTable.SymbolTable
import SymbolTable.SemanticAnalyzerLiteral

-- | The 'buildSymbolTable' function takes a 'Program' and an initial 'SymbolTable',
-- and returns an updated 'SymbolTable' after processing all the global statements in the program.
--
-- The 'Program' type represents a collection of global statements.
-- The function uses a left fold ('foldl') to process each global statement,
-- updating the symbol table incrementally.
buildSymbolTable :: Program -> SymbolTable -> SymbolTable
buildSymbolTable (Program stmts) table = foldl processGlobalStatement table stmts

-- | The 'processGlobalStatement' function processes a single 'GlobalStatement' and updates the 'SymbolTable'.
--
-- Takes the following arguments:
--   - 'SymbolTable': The current symbol table.
--   - 'GlobalStatement': The global statement to process.
--
-- Returns an updated 'SymbolTable' after processing the statement.
--
-- The function delegates to 'processStatement' if the global statement is a regular statement,
-- and leaves the table unchanged for other types of global statements.
processGlobalStatement :: SymbolTable -> GlobalStatement -> SymbolTable
processGlobalStatement table (Statement stmt) = processStatement stmt table
processGlobalStatement table _ = table

-- | The 'processStatement' function processes a single 'Statement' and updates the 'SymbolTable'.
--
-- Takes the following arguments:
--   - 'Statement': The statement to process.
--   - 'SymbolTable': The current symbol table.
--
-- Returns an updated 'SymbolTable' after processing the statement.
--
-- If the statement is an 'ExpressionStatement' containing a literal expression,
-- the function delegates to 'processLiteral' to handle the literal.
-- For other types of statements, the symbol table is left unchanged.
processStatement :: Statement -> SymbolTable -> SymbolTable
processStatement (ExpressionStatement (LiteralExpr lit)) table = processLiteral lit table
processStatement _ table = table