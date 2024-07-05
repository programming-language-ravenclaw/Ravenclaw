module CodeGenerator.CodeGeneratorDataTypeDeclaration.CodeGeneratorDataTypeDeclaration (
    generateDataTypeDeclaration,
    generateDataTypeIntLit,
    generateDataTypeIntArith,
    generateDataTypeFloatLit,
    generateDataTypeFloatArith
) where

import SymbolTable.SymbolTable
import AST.AST
import CodeGenerator.CodeGeneratorArithmetic.CodeGeneratorArithmetic (generateArithmetic)
import CodeGenerator.CodeGeneratorLiteral.CodeGeneratorLiteral (generateLiteral)

generateDataTypeDeclaration :: DataTypeDeclaration -> SymbolTable -> String
generateDataTypeDeclaration (DataTypeDeclarationInt decInt) table = generateDataTypeInt decInt table
generateDataTypeDeclaration (DataTypeDeclarationFloat decFloat) table = generateDataTypeFloat decFloat table
generateDataTypeDeclaration (DataTypeDeclarationString decString) table = generateDataTypeString decString table
generateDataTypeDeclaration _ _ = error "Unsupported DataTypeDeclaration"

generateDataTypeInt :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeInt (DataTypeDecIntLit typeData ident lits) table =
    generateDataTypeIntLit (DataTypeDecIntLit typeData ident lits) table
generateDataTypeInt (DataTypeDecIntArith typeData ident ariths) table =
    generateDataTypeIntArith (DataTypeDecIntArith typeData ident ariths) table

generateDataTypeIntLit :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntLit (DataTypeDecIntLit (DataInt typeData) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident ++ " = 0"  
        [IntegerLiteral lit] -> ident ++ " = " ++ generateLiteral (IntLit (IntegerLiteral lit)) table

generateDataTypeIntArith :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntArith (DataTypeDecIntArith (DataInt typeData) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = 0"  
        _  -> let arithCode = generateArithmetic (IntArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode
generateDataTypeIntArith _ _ = error "Unsupported DataTypeDeclarationInt"

generateDataTypeFloat :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloat (DataTypeDecFloatLit typeData ident lits) table =
    generateDataTypeFloatLit (DataTypeDecFloatLit typeData ident lits) table
generateDataTypeFloat (DataTypeDecFloatArith typeData ident ariths) table =
    generateDataTypeFloatArith (DataTypeDecFloatArith typeData ident ariths) table

generateDataTypeFloatLit :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloatLit (DataTypeDecFloatLit (DataFloat typeData) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident ++ " = 0.0"  
        [FloatLiteral lit] -> ident ++ " = " ++ generateLiteral (FloatLit (FloatLiteral lit)) table

generateDataTypeFloatArith :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloatArith (DataTypeDecFloatArith (DataFloat typeData) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = 0.0"  
        _  -> let arithCode = generateArithmetic (FloatArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode
generateDataTypeFloatArith _ _ = error "Unsupported DataTypeDeclarationFloat"

generateDataTypeStringLit :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeStringLit (DataTypeDecStringLit (DataString typeData) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident  
        [StringLiteral lit] -> ident ++ " = " ++ generateLiteral (StrLit (StringLiteral lit)) table


generateDataTypeStringArith :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeStringArith (DataTypeDecStringArith (DataString typeData) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = \"\""  
        _  -> let arithCode = generateArithmetic (StringArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode

generateDataTypeString :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeString (DataTypeDecStringLit typeData ident lits) table =
    generateDataTypeStringLit (DataTypeDecStringLit typeData ident lits) table
generateDataTypeString (DataTypeDecStringArith typeData ident ariths) table =
    generateDataTypeStringArith (DataTypeDecStringArith typeData ident ariths) table
