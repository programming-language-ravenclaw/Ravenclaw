{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SymbolTable.SemanticAnalyzer.SemanticAnalyzerDataTypeDeclaration.SemanticAnalyzerDataTypeDecla where

import AST.AST
import SymbolTable.SymbolTable

processDataTypeDeclaration :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclaration (DataTypeDeclarationInt (DataTypeDecIntLit typeData identi lit)) table = 
    processDataTypeDeclarationIntLit (DataTypeDeclarationInt (DataTypeDecIntLit typeData identi lit)) table
processDataTypeDeclaration (DataTypeDeclarationInt dataTypeDecIntArith@(DataTypeDecIntArith _ _ _)) table = 
    processDataTypeDeclarationArith (DataTypeDeclarationInt dataTypeDecIntArith) table

processDataTypeDeclarationIntLit :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationIntLit dataType@(DataTypeDeclarationInt (DataTypeDecIntLit _ _ _)) table =
    let symbolInfo = SymbolInfo "intDecLit" "global" (Just (show dataType))
        table' = insertSymbol ("intDecLit" ++ show dataType) symbolInfo table
    in table'

processDataTypeDeclarationArith :: DataTypeDeclaration -> SymbolTable -> SymbolTable
processDataTypeDeclarationArith dataType@(DataTypeDeclarationInt (DataTypeDecIntArith _ _ _)) table =
    let symbolInfo = SymbolInfo "intDecArith" "global" (Just (show dataType))
        table' = insertSymbol ("intDecArith" ++ show dataType) symbolInfo table
    in table'
