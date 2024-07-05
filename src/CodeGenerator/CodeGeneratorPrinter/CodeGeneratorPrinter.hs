{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module CodeGenerator.CodeGeneratorPrinter.CodeGeneratorPrinter (
    generatePrinter
) where

getPrint :: Maybe String -> String
getPrint (Just x) = "print(" ++ x ++ ")"
getPrint Nothing = ""

-- | Generates code for a 'Printer' expression.
--
-- This function converts a 'Printer' expression into a string representation.
--
-- Example:
--
-- >>> let printer = Print (Literal "Hello, World!")
-- >>> let printer = Print (floatLit 3.14)
-- >>> let table = emptyTable
-- >>> generatePrinter printer table
-- "print(\"Hello, World!\")"
generatePrinter :: Maybe String -> String
generatePrinter printer = prints
    where 
        prints = getPrint printer


