module CodeGenerator.CodeGenerator (
    generateCode,
    generateStatement,
    generateStatement'
) where

import SymbolTable.SymbolTable
import AST.AST
import CodeGenerator.CodeGeneratorLiteral.CodeGeneratorLiteral
import CodeGenerator.CodeGeneratorArithmetic.CodeGeneratorArithmetic

-- | Generates code for a whole program based on its AST representation.
--
-- This function generates code for a program by processing each statement and combining them into a single string.
generateCode :: Program -> SymbolTable -> String
generateCode (Program stmts) table = unlines $ map (generateStatement table) stmts

-- | Generates code for a global statement within a program.
--
-- This function dispatches to specific statement generators based on the type of global statement.
generateStatement :: SymbolTable -> GlobalStatement -> String
generateStatement table (Statement stmt) = generateStatement' table stmt
generateStatement _ _ = ""

-- | Generates code for an individual statement within a program.
--
-- This function converts different types of statements into code strings, delegating to specialized generators.
generateStatement' :: SymbolTable -> Statement -> String
generateStatement' table (ExpressionStatement (LiteralExpr lit)) = generateLiteral lit table
generateStatement' table (ExpressionStatement (ArithmeticExpr arithExpr)) = generateArithmetic arithExpr table
generateStatement' _ _ = ""