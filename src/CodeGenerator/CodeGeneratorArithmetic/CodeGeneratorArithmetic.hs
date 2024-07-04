module CodeGenerator.CodeGeneratorArithmetic.CodeGeneratorArithmetic (
    generateArithmetic,
    generateIntArithmetic,
    generateFloatArithmetic,
    generateStringArithmetic,
    generateMixedArithmetic,
    extractFloatValue,
    extractStringValue
) where

import SymbolTable.SymbolTable
import AST.AST
import Utils.ExtractLiteral

-- | Main function that generates code for arithmetic expressions based on the expression type.
--
generateArithmetic :: ArithmeticExpression -> SymbolTable -> String
generateArithmetic (IntArithmetic intArith) table = generateIntArithmetic intArith table
generateArithmetic (FloatArithmetic floatArith) table = generateFloatArithmetic floatArith table
generateArithmetic (StringArithmetic strArith) table = generateStringArithmetic strArith table
generateArithmetic (MixedArithmetic mixedArith) table = generateMixedArithmetic mixedArith table

-- | Generates code for integer arithmetic expressions.
--
-- This function converts an 'IntArithmetic' expression into a code string.
--
-- Example:
--
-- >>> let expr = IntArith (Digit 3) Add (Digit 5) [OpAndDigit Sub (Digit 2)]
-- >>> let table = emptyTable
-- >>> generateIntArithmetic expr table
-- "3 + 5 - 2"
generateIntArithmetic :: IntArithmetic -> SymbolTable -> String
generateIntArithmetic (IntArith digit1 op digit2 ops) table =
    let initialResult = show (extractDigitValue digit1) ++ " " ++ show op ++ " " ++ show (extractDigitValue digit2)
        finalCode = foldl (\acc (OpAndDigit o d) -> acc ++ " " ++ show o ++ " " ++ show (extractDigitValue d)) initialResult ops
    in finalCode

-- | Generates code for float arithmetic expressions.
--
-- This function converts a 'FloatArithmetic' expression into a code string.
--
-- Example:
--
-- >>> let expr = FloatArith (FloatLit 3.0) Add (FloatLit 5.5) [OpAndFloat Sub (FloatLit 2.1)]
-- >>> let table = emptyTable
-- >>> generateFloatArithmetic expr table
-- "3.0 + 5.5 - 2.1"
generateFloatArithmetic :: FloatArithmetic -> SymbolTable -> String
generateFloatArithmetic (FloatArith lit1 op lit2 ops) table =
    let initialResult = show (extractFloatValue lit1) ++ " " ++ show op ++ " " ++ show (extractFloatValue lit2)
        finalCode = foldl (\acc (OpAndFloat o l) -> acc ++ " " ++ show o ++ " " ++ show (extractFloatValue l)) initialResult ops
    in finalCode

-- | Generates code for string arithmetic expressions.
--
-- This function converts a 'StringArithmetic' expression into a code string.
--
-- Example:
--
-- >>> let expr = StringArith (StringLit "hello") Concat (StringLit "world") [OpAndString Concat (StringLit "!")]
-- >>> let table = emptyTable
-- >>> generateStringArithmetic expr table
-- "\"hello\" ++ \"world\" ++ \"!\""
generateStringArithmetic :: StringArithmetic -> SymbolTable -> String
generateStringArithmetic (StringArith lit1 op lit2 ops) table =
    let initialResult = show (extractStringValue lit1) ++ " " ++ show op ++ " " ++ show (extractStringValue lit2)
        finalCode = foldl (\acc (OpAndString o s) -> acc ++ " " ++ show o ++ " " ++ show (extractStringValue s)) initialResult ops
    in finalCode

-- | Generates code for mixed arithmetic expressions.
--
-- This function converts a 'MixedArithmetic' expression into a code string.
--
-- Example 1 (Mixed with digit first):
--
-- >>> let expr = DigitMixed (Digit 3) Add (FloatLit 5.5) [OpAndMixedDigit Sub (Digit 2)]
-- >>> let table = emptyTable
-- >>> generateMixedArithmetic expr table
-- "3 + 5.5"
--
-- Example 2 (Mixed with float first):
--
-- >>> let expr = FloatMixed (FloatLit 3.0) Add (Digit 5) [OpAndMixedFloat Sub (FloatLit 2.1)]
-- >>> let table = emptyTable
-- >>> generateMixedArithmetic expr table
-- "3.0 + 5"
generateMixedArithmetic :: MixedArithmetic -> SymbolTable -> String
generateMixedArithmetic (DigitMixed digit op lit ops) table =
    let initialResult = show (extractDigitValue digit) ++ " " ++ show op ++ " " ++ show (extractFloatValue lit)
        finalCode = foldl (\acc (OpAndMixedDigit o d) -> acc ++ " " ++ show o ++ " " ++ show (extractDigitValue d)) initialResult ops
    in finalCode
generateMixedArithmetic (FloatMixed lit op digit ops) table =
    let initialResult = show (extractFloatValue lit) ++ " " ++ show op ++ " " ++ show (extractDigitValue digit)
        finalCode = foldl (\acc (OpAndMixedFloat o l) -> acc ++ " " ++ show o ++ " " ++ show (extractFloatValue l)) initialResult ops
    in finalCode