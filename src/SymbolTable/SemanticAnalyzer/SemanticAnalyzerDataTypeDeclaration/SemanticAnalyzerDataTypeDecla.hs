{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SymbolTable.SemanticAnalyzer.SemanticAnalyzerDataTypeDeclaration.SemanticAnalyzerDataTypeDecla (
    processDataTypeDeclaration
) where

import AST.AST
import SymbolTable.SymbolTable

-- | Processes a data type declaration and updates the symbol table accordingly.
processDataTypeDeclaration :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclaration (DataTypeDeclarationInt (DataTypeDecIntLit typeData identi lit)) table = 
    processDataTypeDeclarationIntLit (DataTypeDeclarationInt (DataTypeDecIntLit typeData identi lit)) table
processDataTypeDeclaration (DataTypeDeclarationInt dataTypeDecIntArith@(DataTypeDecIntArith _ _ _)) table = 
    processDataTypeDeclarationIntArith (DataTypeDeclarationInt dataTypeDecIntArith) table

processDataTypeDeclaration (DataTypeDeclarationFloat (DataTypeDecFloatLit typeData identi lit)) table =
    processDataTypeDeclarationFloatLit (DataTypeDeclarationFloat (DataTypeDecFloatLit typeData identi lit)) table

processDataTypeDeclaration (DataTypeDeclarationFloat dataTypeDecFloatArith@(DataTypeDecFloatArith _ _ _)) table =
    processDataTypeDeclarationFloatArith (DataTypeDeclarationFloat dataTypeDecFloatArith) table

processDataTypeDeclaration (DataTypeDeclarationBool (DataTypeDecBool typeData identi lit)) table =
    processDataTypeDeclarationBool (DataTypeDeclarationBool (DataTypeDecBool typeData identi lit)) table

processDataTypeDeclaration (DataTypeDeclarationString (DataTypeDecStringLit typeData identi lit)) table =
    processDataTypeDeclarationStringLit (DataTypeDeclarationString (DataTypeDecStringLit typeData identi lit)) table

processDataTypeDeclaration (DataTypeDeclarationString dataTypeDecStringArith@(DataTypeDecStringArith _ _ _)) table =
    processDataTypeDeclarationStringArith (DataTypeDeclarationString dataTypeDecStringArith) table

processDataTypeDeclaration (DataTypeDeclarationList (DataTypeDecList typeData identi lit)) table =
    processDataTypeDeclarationList (DataTypeDeclarationList (DataTypeDecList typeData identi lit)) table

-- | Processes a DataTypeDeclarationInt with DataTypeDecIntLit and updates the symbol table.
processDataTypeDeclarationIntLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationIntLit dataType@(DataTypeDeclarationInt (DataTypeDecIntLit _ _ _)) table =
    let symbolName = "intDecLit" ++ show dataType
        symbolInfo = SymbolInfo "Integer Declaration Literal" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationInt with DataTypeDecIntArith and updates the symbol table.
processDataTypeDeclarationIntArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationIntArith dataType@(DataTypeDeclarationInt (DataTypeDecIntArith _ _ _)) table =
    let symbolName = "intDecArith" ++ show dataType
        symbolInfo = SymbolInfo "Integer Declaration Arithmetic" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationFloat with DataTypeDecFloatLit and updates the symbol table.
processDataTypeDeclarationFloatLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationFloatLit dataType@(DataTypeDeclarationFloat (DataTypeDecFloatLit _ _ _)) table =
    let symbolName = "floatDecLit" ++ show dataType
        symbolInfo = SymbolInfo "Float Declaration Literal" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationFloat with DataTypeDecFloatArith and updates the symbol table.
processDataTypeDeclarationFloatArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationFloatArith dataType@(DataTypeDeclarationFloat (DataTypeDecFloatArith _ _ _)) table =
    let symbolName = "floatDecArith" ++ show dataType
        symbolInfo = SymbolInfo "Float Declaration Arithmetic" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationString with DataTypeDecStringLit and updates the symbol table.
processDataTypeDeclarationStringLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationStringLit dataType@(DataTypeDeclarationString (DataTypeDecStringLit _ _ _)) table =
    let symbolName = "stringDecLit" ++ show dataType
        symbolInfo = SymbolInfo "String Declaration Literal" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationString with DataTypeDecStringArith and updates the symbol table.
processDataTypeDeclarationStringArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationStringArith dataType@(DataTypeDeclarationString (DataTypeDecStringArith _ _ _)) table =
    let symbolName = "stringDecArith" ++ show dataType
        symbolInfo = SymbolInfo "String Declaration Arithmetic" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationBool with DataTypeDecBool and updates the symbol table.
processDataTypeDeclarationBool :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationBool dataType@(DataTypeDeclarationBool (DataTypeDecBool _ _ _)) table =
    let symbolName = "boolDecLit" ++ show dataType
        symbolInfo = SymbolInfo "Boolean Declaration Literal" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'

-- | Processes a DataTypeDeclarationList with DataTypeDecList and updates the symbol table.
processDataTypeDeclarationList :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationList dataType@(DataTypeDeclarationList (DataTypeDecList _ _ _)) table =
    let symbolName = "listDecLit" ++ show dataType
        symbolInfo = SymbolInfo "List Declaration Literal" (Just (show dataType))
        table' = insertSymbol symbolName symbolInfo table
    in table'
