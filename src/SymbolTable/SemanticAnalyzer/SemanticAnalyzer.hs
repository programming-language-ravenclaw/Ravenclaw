module SymbolTable.SemanticAnalyzer.SemanticAnalyzer where

import AST.AST
import SymbolTable.SymbolTable
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerLiteral.SemanticAnalyzerLiteral
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmetic


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
-- If the statement is an 'ExpressionStatement' containing an expression,
-- the function delegates to 'processExpression' to handle the expression.
-- For other types of statements, the symbol table is left unchanged.
processStatement :: Statement -> SymbolTable -> SymbolTable
processStatement (ExpressionStatement expr) table = processExpression expr table
processStatement _ table = table

-- | The 'processExpression' function processes a single 'Expression' and updates the 'SymbolTable'.
--
-- Takes the following arguments:
--   - 'Expression': The expression to process.
--   - 'SymbolTable': The current symbol table.
--
-- Returns an updated 'SymbolTable' after processing the expression.
processExpression :: Expression -> SymbolTable -> SymbolTable
processExpression (LiteralExpr lit) table = processLiteral lit table
processExpression (ArithmeticExpr arithExpr) table = processArithmeticExpression arithExpr table
processExpression _ table = table