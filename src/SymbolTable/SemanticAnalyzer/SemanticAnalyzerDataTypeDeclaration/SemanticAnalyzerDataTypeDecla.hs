{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SymbolTable.SemanticAnalyzer.SemanticAnalyzerDataTypeDeclaration.SemanticAnalyzerDataTypeDecla (processDataTypeDeclaration)
where

import AST.AST
import SymbolTable.SymbolTable

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

processDataTypeDeclarationIntLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationIntLit dataType@(DataTypeDeclarationInt (DataTypeDecIntLit _ _ _)) table =
    let symbolInfo = SymbolInfo "intDecLit" (Just (show dataType))
        table' = insertSymbol ("intDecLit" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationIntArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationIntArith dataType@(DataTypeDeclarationInt (DataTypeDecIntArith _ _ _)) table =
    let symbolInfo = SymbolInfo "intDecArith" (Just (show dataType))
        table' = insertSymbol ("intDecArith" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationFloatLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationFloatLit dataType@(DataTypeDeclarationFloat (DataTypeDecFloatLit _ _ _)) table =
    let symbolInfo = SymbolInfo "floatDecLit" (Just (show dataType))
        table' = insertSymbol ("floatDecLit" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationFloatArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationFloatArith dataType@(DataTypeDeclarationFloat (DataTypeDecFloatArith _ _ _)) table =
    let symbolInfo = SymbolInfo "floatDecArith" (Just (show dataType))
        table' = insertSymbol ("floatDecArith" ++ show dataType) symbolInfo table
    in table'


processDataTypeDeclarationStringLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationStringLit dataType@(DataTypeDeclarationString (DataTypeDecStringLit _ _ _)) table =
    let symbolInfo = SymbolInfo "stringDecLit" (Just (show dataType))
        table' = insertSymbol ("stringDecLit" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationStringArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationStringArith dataType@(DataTypeDeclarationString (DataTypeDecStringArith _ _ _)) table =
    let symbolInfo = SymbolInfo "stringDecArith" (Just (show dataType))
        table' = insertSymbol ("stringDecArith" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationBool :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationBool dataType@(DataTypeDeclarationBool (DataTypeDecBool _ _ _)) table =
    let symbolInfo = SymbolInfo "boolDecLit" (Just (show dataType))
        table' = insertSymbol ("boolDecLit" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationList :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationList dataType@(DataTypeDeclarationList (DataTypeDecList _ _ _)) table =
    let symbolInfo = SymbolInfo "listDecLit" (Just (show dataType))
        table' = insertSymbol ("listDecLit" ++ show dataType) symbolInfo table
    in table'
