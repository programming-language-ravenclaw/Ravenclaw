module CodeGenerator.CodeGeneratorComments.CodeGeneratorComments (
    generateComment
) where

import SymbolTable.SymbolTable
import AST.AST
import Utils.ExtractLiteral


-- | Generates Python code for a comment.
--
-- This function converts a comment into its corresponding Python representation.
-- For line comments, it generates a single-line comment.
-- For block comments, it generates a multi-line comment.
--
-- Example 1: Line Comment
--
-- >>> let comment = LineComment "This is a line comment"
-- >>> generateComment comment emptyTable
-- "# This is a line comment"
--
-- Example 2: Block Comment
--
-- >>> let comment = BlockComment "This is a block comment"
-- >>> generateComment comment emptyTable
-- "\"\"\"This is a block comment\"\"\""
generateComment :: Comment -> SymbolTable -> String
generateComment (LineComment value) _ = "# " ++ value
generateComment (BlockComment value) _ = "\"\"\"" ++ value ++ "\"\"\""
