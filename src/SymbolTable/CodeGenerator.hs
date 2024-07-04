module SymbolTable.CodeGenerator (
    generateCode,
    generateStatement,
    generateStatement',
    generateLiteral
) where

import SymbolTable.SymbolTable
import AST.AST

-- Función principal para generar el código Python.
generateCode :: Program -> SymbolTable -> String
generateCode (Program stmts) table = unlines $ map (generateStatement table) stmts

-- Genera código para una declaración global.
generateStatement :: SymbolTable -> GlobalStatement -> String
generateStatement table (Statement stmt) = generateStatement' table stmt
generateStatement _ _ = ""

-- Genera código para una declaración.
generateStatement' :: SymbolTable -> Statement -> String
generateStatement' table (ExpressionStatement (LiteralExpr lit)) = generateLiteral lit table
generateStatement' table (ExpressionStatement (ArithmeticExpr arithExpr)) = generateArithmetic arithExpr table
generateStatement' _ _ = ""

-- Genera código para un literal.
generateLiteral :: Literal -> SymbolTable -> String
generateLiteral (IntLit (IntegerLiteral value)) _ = show value
generateLiteral (FloatLit (FloatLiteral value)) _ = show value
generateLiteral (BoolLit (BooleanLiteral value)) _ = show value
generateLiteral (StrLit (StringLiteral value)) _ = show value
generateLiteral _ _ = ""

-- Genera código para una expresión aritmética.
generateArithmetic :: ArithmeticExpression -> SymbolTable -> String
generateArithmetic (IntArithmetic intArith) table = generateIntArithmetic intArith table
generateArithmetic (FloatArithmetic floatArith) table = generateFloatArithmetic floatArith table
generateArithmetic (StringArithmetic strArith) table = generateStringArithmetic strArith table
generateArithmetic (MixedArithmetic mixedArith) table = generateMixedArithmetic mixedArith table

-- Genera código para una expresión aritmética de enteros.
generateIntArithmetic :: IntArithmetic -> SymbolTable -> String
generateIntArithmetic (IntArith digit1 op digit2 ops) table =
    let initialResult = show (extractIntValue digit1) ++ " " ++ show op ++ " " ++ show (extractIntValue digit2)
        finalCode = foldl (\acc (OpAndDigit o d) -> acc ++ " " ++ show o ++ " " ++ show (extractIntValue d)) initialResult ops
    in finalCode

-- Genera código para una expresión aritmética de flotantes.
generateFloatArithmetic :: FloatArithmetic -> SymbolTable -> String
generateFloatArithmetic (FloatArith lit1 op lit2 ops) table =
    let initialResult = show (extractFloatValue lit1) ++ " " ++ show op ++ " " ++ show (extractFloatValue lit2)
        finalCode = foldl (\acc (OpAndFloat o l) -> acc ++ " " ++ show o ++ " " ++ show (extractFloatValue l)) initialResult ops
    in finalCode

-- Genera código para una expresión aritmética de cadenas.
generateStringArithmetic :: StringArithmetic -> SymbolTable -> String
generateStringArithmetic (StringArith lit1 op lit2 ops) table =
    let initialResult = show (extractStringValue lit1) ++ " " ++ show op ++ " " ++ show (extractStringValue lit2)
        finalCode = foldl (\acc (OpAndString o s) -> acc ++ " " ++ show o ++ " " ++ show (extractStringValue s)) initialResult ops
    in finalCode

-- Genera código para una expresión aritmética mixta.
generateMixedArithmetic :: MixedArithmetic -> SymbolTable -> String
generateMixedArithmetic (DigitMixed digit op lit ops) table =
    let initialResult = show (extractIntValue digit) ++ " " ++ show op ++ " " ++ show (extractFloatValue lit)
        finalCode = foldl (\acc (OpAndMixedDigit o d) -> acc ++ " " ++ show o ++ " " ++ show (extractIntValue d)) initialResult ops
    in finalCode
generateMixedArithmetic (FloatMixed lit op digit ops) table =
    let initialResult = show (extractFloatValue lit) ++ " " ++ show op ++ " " ++ show (extractIntValue digit)
        finalCode = foldl (\acc (OpAndMixedFloat o l) -> acc ++ " " ++ show o ++ " " ++ show (extractFloatValue l)) initialResult ops
    in finalCode

-- Funciones auxiliares para extraer valores.
extractIntValue :: Digit -> Integer
extractIntValue (Digit d) = d

extractFloatValue :: FloatLiteral -> Float
extractFloatValue (FloatLiteral f) = f

extractStringValue :: StringLiteral -> String
extractStringValue (StringLiteral s) = s
