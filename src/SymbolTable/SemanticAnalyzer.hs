module SymbolTable.SemanticAnalyzer where

import AST.AST
import SymbolTable.SymbolTable
import qualified Data.Map as Map
import Debug.Trace (trace)


buildSymbolTable :: Program -> SymbolTable -> SymbolTable
buildSymbolTable (Program stmts) table = foldl processGlobalStatement table stmts

processGlobalStatement :: SymbolTable -> GlobalStatement -> SymbolTable
processGlobalStatement table (Statement stmt) = processStatement stmt table
processGlobalStatement table _ = table -- Ignorar otros tipos de declaraciones globales

processStatement :: Statement -> SymbolTable -> SymbolTable
processStatement (ExpressionStatement (LiteralExpr lit)) table = processLiteral lit table
processStatement (LiteralStatement lit) table = processLiteral lit table
processStatement _ table = table -- Ignorar otros tipos de declaraciones

processLiteral :: Literal -> SymbolTable -> SymbolTable
processLiteral lit@(IntLit (IntegerLiteral value)) table = 
    let symbolInfo = SymbolInfo "int" "global" Nothing (Just (show value))
        table' = insertSymbol ("intLiteral_" ++ show value) symbolInfo table
    -- in trace (show lit) table'
    in table'
processLiteral lit@(FloatLit (FloatLiteral value)) table = 
    let symbolInfo = SymbolInfo "float" "global" Nothing (Just (show value))
        table' = insertSymbol ("floatLiteral_" ++ show value) symbolInfo table
    -- in trace (show lit) table'
    in table'
processLiteral lit@(BoolLit (BooleanLiteral value)) table = 
    let symbolInfo = SymbolInfo "bool" "global" Nothing (Just (show value))
        table' = insertSymbol ("boolLiteral_" ++ show value) symbolInfo table
    -- in trace (show lit) table'
    in table'
processLiteral lit@(StrLit (StringLiteral value)) table = 
    let symbolInfo = SymbolInfo "string" "global" Nothing (Just value)
        table' = insertSymbol ("stringLiteral_" ++ value) symbolInfo table
    -- in trace (show lit) table'
    in table'