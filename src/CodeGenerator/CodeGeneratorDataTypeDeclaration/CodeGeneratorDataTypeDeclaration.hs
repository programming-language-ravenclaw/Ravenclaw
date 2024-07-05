{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

-- | Generates code for a DataTypeDeclaration, returning the generated code as a string.
generateDataTypeDeclaration :: DataTypeDeclaration -> SymbolTable -> String
generateDataTypeDeclaration (DataTypeDeclarationInt decInt) table = generateDataTypeInt decInt table
generateDataTypeDeclaration (DataTypeDeclarationFloat decFloat) table = generateDataTypeFloat decFloat table
generateDataTypeDeclaration (DataTypeDeclarationString decString) table = generateDataTypeString decString table
generateDataTypeDeclaration _ _ = error "Unsupported DataTypeDeclaration"

-- | Generates code for a DataTypeDeclarationInt, handling literal integers.
generateDataTypeInt :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeInt (DataTypeDecIntLit typeData ident lits) table =
    generateDataTypeIntLit (DataTypeDecIntLit typeData ident lits) table
generateDataTypeInt (DataTypeDecIntArith typeData ident ariths) table =
    generateDataTypeIntArith (DataTypeDecIntArith typeData ident ariths) table

-- | Generates code for a DataTypeDecIntLit, demonstrating integer literal assignment.
generateDataTypeIntLit :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntLit (DataTypeDecIntLit (DataInt _) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident ++ " = 0"  -- Example: "x = 0"
        [IntegerLiteral lit] -> ident ++ " = " ++ generateLiteral (IntLit (IntegerLiteral lit)) table
                                -- Example: "x = 42"

-- | Generates code for a DataTypeDecIntArith, showing integer arithmetic operation assignment.
generateDataTypeIntArith :: DataTypeDeclarationInt -> SymbolTable -> String
generateDataTypeIntArith (DataTypeDecIntArith (DataInt _) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = 0"  -- Example: "y = 0"
        _  -> let arithCode = generateArithmetic (IntArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode
              -- Example: "y = 22 + 12"

-- | Generates code for a DataTypeDeclarationFloat, handling literal floats.
generateDataTypeFloat :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloat (DataTypeDecFloatLit typeData ident lits) table =
    generateDataTypeFloatLit (DataTypeDecFloatLit typeData ident lits) table
generateDataTypeFloat (DataTypeDecFloatArith typeData ident ariths) table =
    generateDataTypeFloatArith (DataTypeDecFloatArith typeData ident ariths) table

-- | Generates code for a DataTypeDecFloatLit, demonstrating float literal assignment.
generateDataTypeFloatLit :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloatLit (DataTypeDecFloatLit (DataFloat _) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident ++ " = 0.0"  -- Example: "z = 0.0"
        [FloatLiteral lit] -> ident ++ " = " ++ generateLiteral (FloatLit (FloatLiteral lit)) table
                              -- Example: "z = 3.14"

-- | Generates code for a DataTypeDecFloatArith, showing float arithmetic operation assignment.
generateDataTypeFloatArith :: DataTypeDeclarationFloat -> SymbolTable -> String
generateDataTypeFloatArith (DataTypeDecFloatArith (DataFloat _) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = 0.0"  -- Example: "w = 0.0"
        _  -> let arithCode = generateArithmetic (FloatArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode
              -- Example: "w = 1.0 * 2.0"

-- | Generates code for a DataTypeDeclarationString, handling literal strings.
generateDataTypeString :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeString (DataTypeDecStringLit typeData ident lits) table =
    generateDataTypeStringLit (DataTypeDecStringLit typeData ident lits) table
generateDataTypeString (DataTypeDecStringArith typeData ident ariths) table =
    generateDataTypeStringArith (DataTypeDecStringArith typeData ident ariths) table

-- | Generates code for a DataTypeDecStringLit, demonstrating string literal assignment.
generateDataTypeStringLit :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeStringLit (DataTypeDecStringLit (DataString _) (Identifier (Letter ident) _) lits) table =
    case lits of
        [] -> ident  -- Example: "str = "" "
        [StringLiteral lit] -> ident ++ " = " ++ generateLiteral (StrLit (StringLiteral lit)) table
                               -- Example: "str = "hello" "

-- | Generates code for a DataTypeDecStringArith, showing string concatenation operation assignment.
generateDataTypeStringArith :: DataTypeDeclarationString -> SymbolTable -> String
generateDataTypeStringArith (DataTypeDecStringArith (DataString _) (Identifier (Letter ident) _) ariths) table =
    case ariths of
        [] -> ident ++ " = \"\""  -- Example: "concatStr = "" "
        _  -> let arithCode = generateArithmetic (StringArithmetic (head ariths)) table
              in ident ++ " = " ++ arithCode
              -- Example: "concatStr = "hola" + "mundo"
