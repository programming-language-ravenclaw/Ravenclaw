module SymbolTable.SemanticAnalyzer.SemanticAnalyzerBoolean.SemanticAnalyzerBoolean where
import Text.Parsec
import Data.Text (pack)
import AST.AST
import SymbolTable.SymbolTable
import Expression.BooleanExpressionParser ( booleanExpression )

-- | Evaluate a boolean expression given a symbol table.
--
-- This function parses and evaluates a boolean expression, using the given symbol table to resolve variable values.
--
-- Example usage:
-- >>> let table = insertSymbol "x" True emptyTable
-- >>> processBooleanExpression table "x && false"
-- Just False
processBooleanExpression :: SymbolTable -> String -> Maybe Bool
processBooleanExpression table expr = case parse booleanExpression "" (pack expr) of
    Left _ -> Nothing
    Right parsedExpr -> Just (eval parsedExpr)
  where
    eval :: BooleanExpression -> Bool
    eval (BooleanExprComparison comp comps) = evalComparison comp && all evalBooleanOpAndComparison comps

    evalBooleanOpAndComparison :: BooleanOpAndComparison -> Bool
    evalBooleanOpAndComparison (BooleanOpComp op comp) = case op of
        And -> evalComparison comp
        Or  -> evalComparison comp

    evalComparison :: ComparisonExpression -> Bool
    evalComparison (LiteralComparison lit rels) = evalLiteralComparison lit rels
    evalComparison (ArithmeticComparison arith rels) = evalArithmeticComparison arith rels
    evalComparison (BooleanComparison boolLit) = evalBooleanLiteral boolLit

    evalLiteralComparison :: LiteralExpression -> [RelationalOpAndLiteral] -> Bool
    evalLiteralComparison lit rels = undefined 

    evalArithmeticComparison :: ArithmeticExpression -> [RelationalOpAndArithmetic] -> Bool
    evalArithmeticComparison arith rels = undefined 

    evalBooleanLiteral :: BooleanLiteral -> Bool
    evalBooleanLiteral (BooleanLiteral val) = val
