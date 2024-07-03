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
processIntArithmetic (IntArith digit1 op digit2 ops) table =
    let table' = insertSymbol ("intArithmetic " ++ show digit1 ++ " " ++ show op ++ " " ++ show digit2) (SymbolInfo "intArithmetic" "local" Nothing) table
    in foldl (\t (OpAndDigit op d) -> insertSymbol ("intOpAndDigit " ++ show op ++ " " ++ show d) (SymbolInfo "intArithmetic" "local" Nothing) t) table' ops

processFloatArithmetic :: FloatArithmetic -> SymbolTable -> SymbolTable
processFloatArithmetic (FloatArith lit1 op lit2 ops) table =
    let table' = insertSymbol ("floatArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) (SymbolInfo "floatArithmetic" "local" Nothing) table
    in foldl (\t (OpAndFloat op l) -> insertSymbol ("floatOpAndFloat " ++ show op ++ " " ++ show l) (SymbolInfo "floatArithmetic" "local" Nothing) t) table' ops

processStringArithmetic :: StringArithmetic -> SymbolTable -> SymbolTable
processStringArithmetic (StringArith lit1 op lit2 ops) table =
    let table' = insertSymbol ("stringArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) (SymbolInfo "stringArithmetic" "local" Nothing) table
    in foldl (\t (OpAndString op s) -> insertSymbol ("stringOpAndString " ++ show op ++ " " ++ show s) (SymbolInfo "stringArithmetic" "local" Nothing) t) table' ops

processMixedArithmetic :: MixedArithmetic -> SymbolTable -> SymbolTable
processMixedArithmetic (DigitMixed digit op lit ops) table =
    let table' = insertSymbol ("mixedArithmeticDigit " ++ show digit ++ " " ++ show op ++ " " ++ show lit) (SymbolInfo "mixedArithmetic" "local" Nothing) table
    in foldl (\t (OpAndMixedDigit op d) -> insertSymbol ("mixedOpAndDigit " ++ show op ++ " " ++ show d) (SymbolInfo "mixedArithmetic" "local" Nothing) t) table' ops
processMixedArithmetic (FloatMixed lit op digit ops) table =
    let table' = insertSymbol ("mixedArithmeticFloat " ++ show lit ++ " " ++ show op ++ " " ++ show digit) (SymbolInfo "mixedArithmetic" "local" Nothing) table
    in foldl (\t (OpAndMixedFloat op l) -> insertSymbol ("mixedOpAndFloat " ++ show op ++ " " ++ show l) (SymbolInfo "mixedArithmetic" "local" Nothing) t) table' ops

{- 

processArithmeticExpression :: ArithmeticExpression -> SymbolTable -> SymbolTable
processArithmeticExpression (IntArithmetic intArith) table = processIntArithmetic intArith table
processArithmeticExpression (FloatArithmetic floatArith) table = processFloatArithmetic floatArith table
processArithmeticExpression (StringArithmetic strArith) table = processStringArithmetic strArith table
processArithmeticExpression (MixedArithmetic mixedArith) table = processMixedArithmetic mixedArith table

processIntArithmetic :: IntArithmetic -> SymbolTable -> SymbolTable
processIntArithmetic (IntArith digit1 op digit2 ops) table =
    let result = evalIntArithmetic (IntArith digit1 op digit2 ops)
        table' = insertSymbol ("intArithmetic " ++ show digit1 ++ " " ++ show op ++ " " ++ show digit2) 
                              (SymbolInfo "intArithmetic" "local" (Just (show result))) table
    in table'

evalIntArithmetic :: IntArithmetic -> Integer
evalIntArithmetic (IntArith (Digit d1) op (Digit d2) ops) =
    foldl (\acc (OpAndDigit o (Digit d)) -> applyOp o acc d) (applyOp op d1 d2) ops

applyOp :: Operator -> Integer -> Integer -> Integer
applyOp Plus     = (+)
applyOp Minus    = (-)
applyOp Multiply = (*)
applyOp Divide   = div

processFloatArithmetic :: FloatArithmetic -> SymbolTable -> SymbolTable
processFloatArithmetic (FloatArith lit1 op lit2 ops) table =
    let result = evalFloatArithmetic (FloatArith lit1 op lit2 ops)
        table' = insertSymbol ("floatArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "floatArithmetic" "local" (Just (show result))) table
    in table'

evalFloatArithmetic :: FloatArithmetic -> Float
evalFloatArithmetic (FloatArith (FloatLiteral f1) op (FloatLiteral f2) ops) =
    foldl (\acc (OpAndFloat o (FloatLiteral f)) -> applyFloatOp o acc f) (applyFloatOp op f1 f2) ops

applyFloatOp :: Operator -> Float -> Float -> Float
applyFloatOp Plus     = (+)
applyFloatOp Minus    = (-)
applyFloatOp Multiply = (*)
applyFloatOp Divide   = (/)

processStringArithmetic :: StringArithmetic -> SymbolTable -> SymbolTable
processStringArithmetic (StringArith lit1 op lit2 ops) table =
    let result = evalStringArithmetic (StringArith lit1 op lit2 ops)
        table' = insertSymbol ("stringArithmetic " ++ show lit1 ++ " " ++ show op ++ " " ++ show lit2) 
                              (SymbolInfo "stringArithmetic" "local" (Just result)) table
    in table'

evalStringArithmetic :: StringArithmetic -> String
evalStringArithmetic (StringArith (StringLiteral s1) op (StringLiteral s2) ops) =
    foldl (\acc (OpAndString o (StringLiteral s)) -> acc ++ s) (s1 ++ s2) ops

processMixedArithmetic :: MixedArithmetic -> SymbolTable -> SymbolTable
processMixedArithmetic (DigitMixed digit op lit ops) table =
    let result = evalMixedArithmetic (DigitMixed digit op lit ops)
        table' = insertSymbol ("mixedArithmeticDigit " ++ show digit ++ " " ++ show op ++ " " ++ show lit) 
                              (SymbolInfo "mixedArithmetic" "local" (Just (show result))) table
    in table'
processMixedArithmetic (FloatMixed lit op digit ops) table =
    let result = evalMixedArithmetic (FloatMixed lit op digit ops)
        table' = insertSymbol ("mixedArithmeticFloat " ++ show lit ++ " " ++ show op ++ " " ++ show digit) 
                              (SymbolInfo "mixedArithmetic" "local" (Just (show result))) table
    in table'

evalMixedArithmetic :: MixedArithmetic -> Float
evalMixedArithmetic (DigitMixed (Digit d) op (FloatLiteral f) ops) =
    foldl (\acc (OpAndMixedDigit o (Digit d')) -> applyFloatOp o acc (fromIntegral d')) (applyFloatOp op (fromIntegral d) f) ops
evalMixedArithmetic (FloatMixed (FloatLiteral f) op (Digit d) ops) =
    foldl (\acc (OpAndMixedFloat o (FloatLiteral f')) -> applyFloatOp o acc f') (applyFloatOp op f (fromIntegral d)) ops


 -}