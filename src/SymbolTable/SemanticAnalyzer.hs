module SymbolTable.SemanticAnalyzer where

import AST.AST
import SymbolTable.SymbolTable
import SymbolTable.SemanticAnalyzerLiteral

-- | The 'buildSymbolTable' function takes a 'Program' and an initial 'SymbolTable',
-- and returns an updated 'SymbolTable' after processing all the global statements in the program.
--
-- The 'Program' type represents a collection of global statements.
-- The function uses a left fold ('foldl') to process each global statement,
-- updating the symbol table incrementally.
buildSymbolTable :: Program -> SymbolTable -> SymbolTable
buildSymbolTable (Program stmts) table = foldl processGlobalStatement table stmts

-- | The 'processGlobalStatement' function processes a single 'GlobalStatement' and updates the 'SymbolTable'.
--
-- Takes the following arguments:
--   - 'SymbolTable': The current symbol table.
--   - 'GlobalStatement': The global statement to process.
--
-- Returns an updated 'SymbolTable' after processing the statement.
--
-- The function delegates to 'processStatement' if the global statement is a regular statement,
-- and leaves the table unchanged for other types of global statements.
processGlobalStatement :: SymbolTable -> GlobalStatement -> SymbolTable
processGlobalStatement table (Statement stmt) = processStatement stmt table
processGlobalStatement table _ = table

-- | The 'processStatement' function processes a single 'Statement' and updates the 'SymbolTable'.
--
-- Takes the following arguments:
--   - 'Statement': The statement to process.
--   - 'SymbolTable': The current symbol table.
--
-- Returns an updated 'SymbolTable' after processing the statement.
--
-- If the statement is an 'ExpressionStatement' containing a literal expression,
-- the function delegates to 'processLiteral' to handle the literal.
-- For other types of statements, the symbol table is left unchanged.
processStatement :: Statement -> SymbolTable -> SymbolTable
processStatement (ExpressionStatement (LiteralExpr lit)) table = processLiteral lit table
processStatement (ExpressionStatement (ArithmeticExpr arithExpr)) table = processArithmeticExpression arithExpr table
processStatement _ table = table

processArithmeticExpression :: ArithmeticExpression -> SymbolTable -> SymbolTable
processArithmeticExpression (IntArithmetic intArith) table = processIntArithmetic intArith table
processArithmeticExpression (FloatArithmetic floatArith) table = processFloatArithmetic floatArith table
processArithmeticExpression (StringArithmetic strArith) table = processStringArithmetic strArith table
processArithmeticExpression (MixedArithmetic mixedArith) table = processMixedArithmetic mixedArith table

processIntArithmetic :: IntArithmetic -> SymbolTable -> SymbolTable
processIntArithmetic arith@(IntArith digit1 op digit2 ops) table =
    let initialResult = applyIntOp op (extractIntValue digit1) (extractIntValue digit2)
        table' = insertSymbol ("intArithmetic " ++ show digit1 ++ " " ++ show op ++ " " ++ show digit2) 
                              (SymbolInfo "intArithmetic" "local" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndDigit o d) -> 
                                let newResult = applyIntOp o result (extractIntValue d)
                                in (insertSymbol ("intOpAndDigit " ++ show o ++ " " ++ show d) 
                                                  (SymbolInfo "intArithmetic" "local" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

applyIntOp :: Operator -> Integer -> Integer -> Integer
applyIntOp Plus     = (+)
applyIntOp Minus    = (-)
applyIntOp Multiply = (*)
applyIntOp Divide   = div

extractIntValue :: Digit -> Integer
extractIntValue (Digit d) = d

processFloatArithmetic :: FloatArithmetic -> SymbolTable -> SymbolTable
processFloatArithmetic arith@(FloatArith lit1 op lit2 ops) table =
    let initialResult = applyFloatOp op (extractFloatValue lit1) (extractFloatValue lit2)
        table' = insertSymbol ("floatArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "floatArithmetic" "local" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndFloat o l) -> 
                                let newResult = applyFloatOp o result (extractFloatValue l)
                                in (insertSymbol ("floatOpAndFloat " ++ show o ++ " " ++ show l) 
                                                  (SymbolInfo "floatArithmetic" "local" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

applyFloatOp :: Operator -> Float -> Float -> Float
applyFloatOp Plus     = (+)
applyFloatOp Minus    = (-)
applyFloatOp Multiply = (*)
applyFloatOp Divide   = (/)

extractFloatValue :: FloatLiteral -> Float
extractFloatValue (FloatLiteral f) = f

processStringArithmetic :: StringArithmetic -> SymbolTable -> SymbolTable
processStringArithmetic arith@(StringArith lit1 op lit2 ops) table =
    let initialResult = applyStringOp op (extractStringValue lit1) (extractStringValue lit2)
        table' = insertSymbol ("stringArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "stringArithmetic" "local" (Just initialResult)) table
        finalTable = foldl (\(acc, result) (OpAndString o s) -> 
                                let newResult = applyStringOp o result (extractStringValue s)
                                in (insertSymbol ("stringOpAndString " ++ show o ++ " " ++ show s) 
                                                  (SymbolInfo "stringArithmetic" "local" (Just newResult)) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable

applyStringOp :: OperatorConcat -> String -> String -> String
applyStringOp Concat = (++)

extractStringValue :: StringLiteral -> String
extractStringValue (StringLiteral s) = s

processMixedArithmetic :: MixedArithmetic -> SymbolTable -> SymbolTable
processMixedArithmetic arith@(DigitMixed digit op lit ops) table =
    let initialResult = applyFloatOp op (fromIntegral (extractIntValue digit)) (extractFloatValue lit)
        table' = insertSymbol ("mixedArithmeticDigit " ++ show digit ++ " " ++ show op ++ " " ++ show lit) 
                              (SymbolInfo "mixedArithmetic" "local" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndMixedDigit o d) -> 
                                let newResult = applyFloatOp o result (fromIntegral (extractIntValue d))
                                in (insertSymbol ("mixedOpAndDigit " ++ show o ++ " " ++ show d) 
                                                  (SymbolInfo "mixedArithmetic" "local" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable
processMixedArithmetic arith@(FloatMixed lit op digit ops) table =
    let initialResult = applyFloatOp op (extractFloatValue lit) (fromIntegral (extractIntValue digit))
        table' = insertSymbol ("mixedArithmeticFloat " ++ show lit ++ " " ++ show op ++ " " ++ show digit) 
                              (SymbolInfo "mixedArithmetic" "local" (Just (show initialResult))) table
        finalTable = foldl (\(acc, result) (OpAndMixedFloat o l) -> 
                                let newResult = applyFloatOp o result (extractFloatValue l)
                                in (insertSymbol ("mixedOpAndFloat " ++ show o ++ " " ++ show l) 
                                                  (SymbolInfo "mixedArithmetic" "local" (Just (show newResult))) acc, newResult)
                           ) (table', initialResult) ops
    in fst finalTable