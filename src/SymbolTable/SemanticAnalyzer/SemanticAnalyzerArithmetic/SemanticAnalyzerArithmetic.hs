module SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmetic where

import AST.AST
import SymbolTable.SymbolTable
import Utils.ExtractLiteral

-- | Processes an arithmetic expression and updates the symbol table with evaluated results.
--
-- This function processes different types of arithmetic expressions and updates the symbol table
-- with the evaluated results of each operation.
processArithmeticExpression :: ArithmeticExpression -> SymbolTable -> SymbolTable
processArithmeticExpression (IntArithmetic intArith) table = processIntArithmetic intArith table
processArithmeticExpression (FloatArithmetic floatArith) table = processFloatArithmetic floatArith table
processArithmeticExpression (StringArithmetic strArith) table = processStringArithmetic strArith table
processArithmeticExpression (MixedArithmetic mixedArith) table = processMixedArithmetic mixedArith table

-- | Processes an integer arithmetic expression and updates the symbol table with evaluated results.
--
-- This function evaluates an 'IntArithmetic' expression and updates the symbol table with intermediate
-- and final results of each operation.
processIntArithmetic :: IntArithmetic -> SymbolTable -> SymbolTable
processIntArithmetic arith@(IntArith digit1 op digit2 ops) table =
    let initialResult = applyIntOp op (extractDigitValue digit1) (extractDigitValue digit2)
        table' = insertSymbol ("intArithmetic " ++ show digit1 ++ " " ++ show op ++ " " ++ show digit2) 
                              (SymbolInfo "intArithmetic" "global" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndDigit o d) -> 
                                let newResult = applyIntOp o result (extractDigitValue d)
                                in (insertSymbol ("intOpAndDigit " ++ show o ++ " " ++ show d) 
                                                  (SymbolInfo "intArithmetic" "global" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

-- | Applies the specified integer operator to two integer operands.
--
-- This function takes an 'Operator' and applies it to two 'Integer' values,
-- returning the result as an 'Integer'.
applyIntOp :: Operator -> Integer -> Integer -> Integer
applyIntOp Plus     = (+)
applyIntOp Minus    = (-)
applyIntOp Multiply = (*)
applyIntOp Divide   = div

-- | Processes a float arithmetic expression and updates the symbol table with evaluated results.
--
-- This function evaluates a 'FloatArithmetic' expression and updates the symbol table with intermediate
-- and final results of each operation.
processFloatArithmetic :: FloatArithmetic -> SymbolTable -> SymbolTable
processFloatArithmetic arith@(FloatArith lit1 op lit2 ops) table =
    let initialResult = applyFloatOp op (extractFloatValue lit1) (extractFloatValue lit2)
        table' = insertSymbol ("floatArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "floatArithmetic" "global" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndFloat o l) -> 
                                let newResult = applyFloatOp o result (extractFloatValue l)
                                in (insertSymbol ("floatOpAndFloat " ++ show o ++ " " ++ show l) 
                                                  (SymbolInfo "floatArithmetic" "global" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

-- | Applies the specified floating-point operator to two float operands.
--
-- This function takes an 'Operator' and applies it to two 'Float' values,
-- returning the result as a 'Float'.
applyFloatOp :: Operator -> Float -> Float -> Float
applyFloatOp Plus     = (+)
applyFloatOp Minus    = (-)
applyFloatOp Multiply = (*)
applyFloatOp Divide   = (/)

-- | Processes a string arithmetic expression and updates the symbol table with evaluated results.
--
-- This function evaluates a 'StringArithmetic' expression and updates the symbol table with intermediate
-- and final results of each operation.
processStringArithmetic :: StringArithmetic -> SymbolTable -> SymbolTable
processStringArithmetic arith@(StringArith lit1 op lit2 ops) table =
    let initialResult = applyStringOp op (extractStringValue lit1) (extractStringValue lit2)
        table' = insertSymbol ("stringArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "stringArithmetic" "global" (Just initialResult)) table
        finalTable = foldl (\(acc, result) (OpAndString o s) -> 
                                let newResult = applyStringOp o result (extractStringValue s)
                                in (insertSymbol ("stringOpAndString " ++ show o ++ " " ++ show s) 
                                                  (SymbolInfo "stringArithmetic" "global" (Just newResult)) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

-- | Concatenates two strings using the 'Concat' operator.
--
-- This function concatenates two 'String' values when the 'OperatorConcat'
-- is 'Concat', returning the resulting concatenated string.
applyStringOp :: OperatorConcat -> String -> String -> String
applyStringOp Concat = (++)

-- | Processes a mixed arithmetic expression with a digit as the first operand.
--
-- This function evaluates a 'MixedArithmetic' expression starting with a digit and updates the symbol table
-- with intermediate and final results of each operation.
processMixedArithmetic :: MixedArithmetic -> SymbolTable -> SymbolTable
processMixedArithmetic arith@(DigitMixed digit op lit ops) table =
    let initialResult = applyFloatOp op (fromIntegral (extractDigitValue digit)) (extractFloatValue lit)
        table' = insertSymbol ("mixedArithmeticDigit " ++ show digit ++ " " ++ show op ++ " " ++ show lit) 
                              (SymbolInfo "mixedArithmetic" "global" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndMixedDigit o d) -> 
                                let newResult = applyFloatOp o result (fromIntegral (extractDigitValue d))
                                in (insertSymbol ("mixedOpAndDigit " ++ show o ++ " " ++ show d) 
                                                  (SymbolInfo "mixedArithmetic" "global" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

-- | Processes a mixed arithmetic expression with a float as the first operand.
--
-- This function evaluates a 'MixedArithmetic' expression starting with a float and updates the symbol table
-- with intermediate and final results of each operation.
processMixedArithmetic arith@(FloatMixed lit op digit ops) table =
    let initialResult = applyFloatOp op (extractFloatValue lit) (fromIntegral (extractDigitValue digit))
        table' = insertSymbol ("mixedArithmeticFloat " ++ show lit ++ " " ++ show op ++ " " ++ show digit) 
                              (SymbolInfo "mixedArithmetic" "global" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndMixedFloat o l) -> 
                                let newResult = applyFloatOp o result (extractFloatValue l)
                                in (insertSymbol ("mixedOpAndFloat " ++ show o ++ " " ++ show l) 
                                                  (SymbolInfo "mixedArithmetic" "global" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable