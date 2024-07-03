module SymbolTable.SemanticAnalyzerIfDiff (
    processConditional
) where

import AST.AST
import SymbolTable.SymbolTable
import SymbolTable.SemanticAnalyzer

processConditional :: ConditionalStatment -> SymbolTable -> SymbolTable
processConditional (IfStatement _ thenStmts diffIfStmts [elseStmt]) table =
    let table' = processStatements thenStmts table
        table'' = processDiffIfStmts diffIfStmts table'
        table''' = processElseStatement elseStmt table''
    in  table'''

processDiffIfStmts :: [DiffIfStatement] -> SymbolTable -> SymbolTable
processDiffIfStmts stmts table
  = foldl (flip processDiffIfStatement) table stmts

processDiffIfStatement :: DiffIfStatement -> SymbolTable -> SymbolTable
processDiffIfStatement (DiffIf _ stmts) = processStatements stmts

processElseStatement :: ElseStatement -> SymbolTable -> SymbolTable
processElseStatement (Else stmts) = processStatements stmts

processStatements :: [Statement] -> SymbolTable -> SymbolTable
processStatements stmts table
  = foldl (flip processStatement) table stmts