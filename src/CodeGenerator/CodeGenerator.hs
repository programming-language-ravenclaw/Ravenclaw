module CodeGenerator.CodeGenerator (
    generateCode,
    generateStatement,
    generateStatement'
) where

import SymbolTable.SymbolTable
import AST.AST
import CodeGenerator.CodeGeneratorLiteral.CodeGeneratorLiteral
    ( generateLiteral )
import CodeGenerator.CodeGeneratorArithmetic.CodeGeneratorArithmetic
import Data.List (intercalate)

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
generateStatement' table (ExpressionStatement expr) = generateExpression expr table
generateStatement' table (ListStatement expr) = generateListExpression expr table
generateStatement' table (MethodCallStatement methodCall) = generateMethodCall methodCall table
generateStatement' _ _ = ""

-- | Generates code for a single element within a list expression.
--
-- Takes the following arguments:
--   - 'SymbolTable': The current symbol table.
--   - 'Expression': The expression to process.
--
-- Returns a string representing the code for the given expression.
generateExpression :: Expression -> SymbolTable -> String
generateExpression (LiteralExpr lit) table = generateLiteral lit table
generateExpression (ArithmeticExpr arithExpr) table = generateArithmetic arithExpr table
generateExpression (ListExpression list) table = generateListExpression list table
generateExpression _ _ = ""

-- | Generates code for a list expression and updates the symbol table with evaluated results.
--
-- This function generates code for a 'ListExpression' by processing each element and combining them into a list.
generateListExpression :: ListExpression -> SymbolTable -> String
generateListExpression (ListExpr exprs) table =
    "[" ++ generateListElements exprs table ++ "]"

-- | Generates code for individual elements within a list expression.
--
-- Takes the following arguments:
--   - 'SymbolTable': The current symbol table.
--   - '[Expression]': The list of expressions to process.
--
-- Returns a string representing the concatenated code for all elements in the list.
generateListElements :: [Expression] -> SymbolTable -> String
generateListElements exprs table =
    let generatedElements = map (\expr -> generateExpression expr table) exprs
    in intercalate ", " generatedElements