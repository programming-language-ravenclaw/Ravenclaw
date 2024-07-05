{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module SymbolTable.SemanticAnalyzer.SemanticAnalyzerPrinter.SemanticAnalyzerPrinter(processPrinter)  where

import AST.AST
import SymbolTable.SymbolTable
import Utils.ExtractLiteral
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmetic

processIntArithmetic' :: IntArithmetic -> String
processIntArithmetic' (IntArith digit1 op digit2 ops) = 
        show $ foldl (\acc (OpAndDigit o d) -> 
            let newResult = applyIntOp o acc (extractDigitValue d)
            in  newResult
        ) result ops
    where 
        digit1' = extractDigitValue digit1
        digit2' = extractDigitValue digit2
        result = applyIntOp op digit1' digit2'

processFloatArithmetic' :: FloatArithmetic -> String
processFloatArithmetic' (FloatArith lit1 op lit2 ops) = 
        show $ foldl (\acc (OpAndFloat o l) -> 
            let newResult = applyFloatOp o acc (extractFloatValue l)
            in newResult
        ) result ops
    where 
        lit1' = extractFloatValue lit1
        lit2' = extractFloatValue lit2
        result = applyFloatOp op lit1' lit2'

processStringArithmetic' :: StringArithmetic -> String
processStringArithmetic' (StringArith lit1 op lit2 ops) = 
        foldl (\acc (OpAndString o l) -> 
            let newResult = applyStringOp o acc (extractStringValue l)
            in newResult
        ) result ops
    where
        lit1' = extractStringValue lit1
        lit2' = extractStringValue lit2
        result = applyStringOp op lit1' lit2'
    
processMixedArithmetic' :: MixedArithmetic -> String
processMixedArithmetic' (DigitMixed digit op lit ops) = 
        show $ foldl (\acc (OpAndMixedDigit o l) -> 
            let newResult = applyFloatOp o acc (fromIntegral (extractDigitValue l))
            in newResult
        ) result ops
    where
        lit1' = fromIntegral (extractDigitValue digit)
        lit2' = extractFloatValue lit
        result = applyFloatOp op lit1' lit2'
processMixedArithmetic' (FloatMixed lit op digit ops) =
        show $ foldl (\acc (OpAndMixedFloat o l) -> 
            let newResult = applyFloatOp o acc (extractFloatValue l)
            in newResult
        ) result ops
    where
        lit1' = extractFloatValue lit
        lit2' = fromIntegral (extractDigitValue digit)
        result = applyFloatOp op lit1' lit2'


--Function processPrinter takes an AST node of type Printer and a current symbol table, and returns an updated symbol table.
--This function handles printing operations in the context of the symbol table.

--Parameters:
-- 'printer': An AST node representing a printing operation. This node is part of the AST structure defined in the AST module.
-- 'symbolTable': The current symbol table before processing the Printer node. The symbol table is a data structure that stores information about expression (such as variables and functions) used in the program.
--Returns: The function returns a SymbolTable, which is the updated symbol table getprocessing  Printer node.
processArithmeticExpression' :: ArithmeticExpression -> String
processArithmeticExpression' (IntArithmetic x) = processIntArithmetic' x
processArithmeticExpression' (FloatArithmetic x) = processFloatArithmetic' x
processArithmeticExpression' (StringArithmetic x) = processStringArithmetic' x
processArithmeticExpression' (MixedArithmetic x) = processMixedArithmetic' x 

processLiteral' :: Literal  -> String
processLiteral' (IntLit (IntegerLiteral value))  = show value
processLiteral' (FloatLit (FloatLiteral value))  = show value
processLiteral' (BoolLit (BooleanLiteral value))  = show value
processLiteral' (StrLit (StringLiteral value))  = show value

processBoolExpr' :: BooleanExpression -> String
processBoolExpr' (BooleanExprComparison (BooleanComparison (BooleanLiteral x)) _) = show x
processBoolExpr' _ = ""

processExpression' :: Expression ->  String
processExpression' _expr@(ArithmeticExpr value) = processArithmeticExpression' value
processExpression' _expr@(LiteralExpr value) = processLiteral' value
processExpression' _expr@(BooleanExpr value) = processBoolExpr' value
processExpression' _  = ""

convertToPrint :: Maybe String -> (String, SymbolInfo)
convertToPrint (Just x) = ("print_" ++ x, SymbolInfo "printer" "global" (Just x))
convertToPrint Nothing = ("print", SymbolInfo "printer" "global" Nothing)

-- | Processes a 'Printer' expression and updates the 'SymbolTable' accordingly.
--
-- The 'Printer' expression is used to create a printer symbol in the symbol table.
-- The printer symbol is inserted with the key "printer_".
--
-- >>> processPrinter (Print (Literal "Hello, World!")) emptySymbolTable
-- SymbolTable { ... }
processPrinter :: Printer -> SymbolTable -> SymbolTable
processPrinter _exp@(Print value) table = 
    let value' = processExpression' value
        (key, symbolInfo) = convertToPrint (Just value')
        table' = insertSymbol key symbolInfo table
    in table'
