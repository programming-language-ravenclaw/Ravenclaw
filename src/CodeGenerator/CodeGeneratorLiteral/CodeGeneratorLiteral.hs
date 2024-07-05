module CodeGenerator.CodeGeneratorLiteral.CodeGeneratorLiteral (
    generateLiteral
) where

import SymbolTable.SymbolTable
import AST.AST

-- | Generates code for literals based on their type.
--
-- This function converts different types of literals into their corresponding string representations.
--
-- Example 1 (Integer literal):
--
-- >>> let lit = IntLit (IntegerLiteral 42)
-- >>> let table = emptyTable
-- >>> generateLiteral lit table
-- "42"
--
-- Example 2 (Float literal):
--
-- >>> let lit = FloatLit (FloatLiteral 3.14)
-- >>> let table = emptyTable
-- >>> generateLiteral lit table
-- "3.14"
--
-- Example 3 (Boolean literal):
--
-- >>> let lit = BoolLit (BooleanLiteral True)
-- >>> let table = emptyTable
-- >>> generateLiteral lit table
-- "True"
--
-- Example 4 (String literal):
--
-- >>> let lit = StrLit (StringLiteral "hello")
-- >>> let table = emptyTable
-- >>> generateLiteral lit table
-- "\"hello\""
generateLiteral :: Literal -> SymbolTable -> String
generateLiteral (IntLit (IntegerLiteral value)) _ = show value
generateLiteral (FloatLit (FloatLiteral value)) _ = show value
generateLiteral (BoolLit (BooleanLiteral value)) _ = show value
generateLiteral (StrLit (StringLiteral value)) _ = show value