module SymbolTable.SematicAnalyzerPrinter (
    processPrinter
) where

import AST.AST
import SymbolTable.SymbolTable

--Function processPrinter takes an AST node of type Printer and a current symbol table, and returns an updated symbol table.
--This function handles printing operations in the context of the symbol table.

--Parameters:
-- 'printer': An AST node representing a printing operation. This node is part of the AST structure defined in the AST module.
-- 'symbolTable': The current symbol table before processing the Printer node. The symbol table is a data structure that stores information about expression (such as variables and functions) used in the program.
--Returns: The function returns a SymbolTable, which is the updated symbol table after processing the Printer node.


-- CreatePrinterSymbolInfo (Just "printer value")
-- SymbolInfo "printer" "global" (Just "printer value")
createPrinterSymbolInfo :: Maybe String -> SymbolInfo
createPrinterSymbolInfo = SymbolInfo "printer" "global"

-- | Processes a 'Printer' expression and updates the 'SymbolTable' accordingly.
--
-- The 'Printer' expression is used to create a printer symbol in the symbol table.
-- The printer symbol is inserted with the key "printer_".
--
-- >>> processPrinter (Print (Literal "Hello, World!")) emptySymbolTable
-- SymbolTable { ... }
processPrinter :: Printer -> SymbolTable -> SymbolTable
processPrinter (Print expr) symbolTable =
    let printerSymbol = createPrinterSymbolInfo (Just "printer value")
        updatedSymbolTable = insertSymbol "printer_" printerSymbol symbolTable
    in updatedSymbolTable

