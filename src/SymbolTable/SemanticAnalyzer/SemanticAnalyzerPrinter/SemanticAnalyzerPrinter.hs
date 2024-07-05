module SymbolTable.SemanticAnalyzer.SemanticAnalyzerPrinter.SemanticAnalyzerPrinter  where

import AST.AST
import SymbolTable.SymbolTable
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmetic (processArithmeticExpression)
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerLiteral.SemanticAnalyzerLiteral (processLiteral)
import Data.Foldable (Foldable(toList))
import qualified Data.Map as Map

--Function processPrinter takes an AST node of type Printer and a current symbol table, and returns an updated symbol table.
--This function handles printing operations in the context of the symbol table.

--Parameters:
-- 'printer': An AST node representing a printing operation. This node is part of the AST structure defined in the AST module.
-- 'symbolTable': The current symbol table before processing the Printer node. The symbol table is a data structure that stores information about expression (such as variables and functions) used in the program.
--Returns: The function returns a SymbolTable, which is the updated symbol table getprocessing  Printer node.

processExpression' :: Expression -> SymbolTable -> SymbolTable
processExpression' _expr@(ArithmeticExpr value) table = processArithmeticExpression value table
processExpression' _expr@(LiteralExpr value) table = processLiteral value table
processExpression' _ table = table

getValues :: SymbolTable -> [Maybe String]
getValues table = map symbolValue (toList table)

convertToPrint :: Maybe String -> (String, SymbolInfo)
convertToPrint (Just x) = ("print_" ++ x, SymbolInfo "print" "global" (Just x))
convertToPrint Nothing = ("print", SymbolInfo "print" "global" Nothing)

-- | Processes a 'Printer' expression and updates the 'SymbolTable' accordingly.
--
-- The 'Printer' expression is used to create a printer symbol in the symbol table.
-- The printer symbol is inserted with the key "printer_".
--
-- >>> processPrinter (Print (Literal "Hello, World!")) emptySymbolTable
-- SymbolTable { ... }
processPrinter :: Printer -> SymbolTable -> SymbolTable
processPrinter _exp@(Print value) table = Map.fromList $ map convertToPrint values
    where
        values = getValues (processExpression' value table)

