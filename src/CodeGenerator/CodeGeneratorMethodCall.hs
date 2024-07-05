module CodeGenerator.CodeGeneratorMethodCall (
    generateMethodCall
) where

import SymbolTable.SymbolTable
import AST.AST
import CodeGenerator.CodeGenerator (generateExpression)
import Data.List (intercalate)

-- | Generates Python code for a method call.
generateMethodCall :: MethodCall -> SymbolTable -> String
generateMethodCall (MethodCall (Identifier (Letter name) _) args) table =
    let argStrs = map (`generateExpression` table) args
    in name ++ "(" ++ intercalate ", " argStrs ++ ")"
