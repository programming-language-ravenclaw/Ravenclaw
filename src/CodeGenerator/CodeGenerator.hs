module CodeGenerator.CodeGenerator (
    generateCode,
    generateStatement,
    generateStatement',
    generateCodeWithStatements
) where

import SymbolTable.SymbolTable
import AST.AST
import CodeGenerator.CodeGeneratorLiteral.CodeGeneratorLiteral
    ( generateLiteral )
import CodeGenerator.CodeGeneratorArithmetic.CodeGeneratorArithmetic
import CodeGenerator.CodeGeneratorComments.CodeGeneratorComments
import CodeGenerator.CodeGeneratorPrinter.CodeGeneratorPrinter
    ( generatePrinter )
import Data.List (intercalate, dropWhileEnd)
import CodeGenerator.CodeGeneratorDataTypeDeclaration.CodeGeneratorDataTypeDeclaration
import qualified Data.Map as Map

trim :: String -> String
trim = dropWhileEnd (== '\n') . dropWhile (== '\n')


-- | Generates code for a whole program based on its AST representation.
--
-- This function generates code for a program by processing each statement and combining them into a single string.
generateCode :: SymbolTable -> String
generateCode table = trim statements
    where
        tablesStatements = map snd (Map.toList table)
        statements = unlines $ map generateStatement tablesStatements

generateCodeWithStatements :: Program -> SymbolTable -> String
generateCodeWithStatements (Program stmts) table = trim statements
    where
        statements = unlines $ map (generateStatementWithStatement table) stmts


-- | Generates code for a global statement within a program.
--
-- This function dispatches to specific statement generators based on the type of global statement.
generateStatementWithStatement :: SymbolTable -> GlobalStatement -> String
generateStatementWithStatement table (Statement stmt) = generateStatement' table stmt
generateStatementWithStatement _ _ = ""

-- | Generates code for a global statement within a program.
--
-- This function dispatches to specific statement generators based on the type of global statement.
generateStatement :: SymbolInfo -> String
generateStatement symbolInfo = 
    case symbolType symbolInfo of
        "printer" -> generatePrinter value
        _ -> ""
    where
        value = symbolValue symbolInfo

-- | Generates code for an individual statement within a program.
--
-- This function converts different types of statements into code strings, delegating to specialized generators.
generateStatement' :: SymbolTable -> Statement -> String
generateStatement' table (ExpressionStatement expr) = generateExpression expr table
generateStatement' table (ListStatement expr) = generateListExpression expr table
generateStatement' table (Comment comment) = generateComment comment table
generateStatement' table (DataTypeDeclarationStatement dataTypeDec) = generateDataTypeDeclaration dataTypeDec table
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