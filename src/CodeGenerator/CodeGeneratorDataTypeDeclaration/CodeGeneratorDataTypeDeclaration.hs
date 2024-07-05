module CodeGenerator.CodeGeneratorDataTypeDeclaration.CodeGeneratorDataTypeDeclaration (
        generateDataTypeDeclaration,
    generateDataTypeIntLit,
    generateDataTypeIntArith
) where

import SymbolTable.SymbolTable
import AST.AST

-- | Main function that generates code for data type declarations based on the declaration type.
--
generateDataTypeDeclaration :: DataTypeDeclaration -> SymbolTable -> String
generateDataTypeDeclaration (DataTypeDeclarationInt decInt) table = generateDataTypeInt decInt table
generateDataTypeDeclaration _ _ = error "Unsupported DataTypeDeclaration"

-- | Generates code for integer data type declarations.
generateDataTypeInt :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeInt (DataTypeDecIntLit typeData ident lits) table = 
    generateDataTypeIntLit (DataTypeDecIntLit typeData ident lits) table
generateDataTypeInt (DataTypeDecIntArith typeData ident ariths) table = 
    generateDataTypeIntArith (DataTypeDecIntArith typeData ident ariths) table

-- | Generates code for integer literal data type declarations.
generateDataTypeIntLit :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntLit (DataTypeDecIntLit (DataInt typeData) (Identifier (Letter ident) _) [IntegerLiteral lit]) table =
    ident ++ " = " ++ show lit

-- | Generates code for integer arithmetic data type declarations.
generateDataTypeIntArith :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntArith (DataTypeDecIntArith (DataInt typeData) (Identifier (Letter ident) _) ariths) table =
    let arithCode = concatMap generateIntArithmetic ariths
    in ident ++ " = " ++ arithCode

-- Helper function to generate code for integer arithmetic expressions.
generateIntArithmetic :: IntArithmetic -> String
generateIntArithmetic (IntArith (Digit d1) op (Digit d2) ops) =
    let initialResult = show d1 ++ " " ++ show op ++ " " ++ show d2
    in foldl (\acc (OpAndDigit o (Digit d)) -> acc ++ " " ++ show o ++ " " ++ show d) initialResult ops
