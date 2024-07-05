module SymbolTable.SemanticAnalyzer.SemanticAnalyzerLiteral.SemanticAnalyzerLiteralTest where

import Test.Hspec
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerLiteral.SemanticAnalyzerLiteral (processLiteral)
import SymbolTable.SymbolTable (SymbolInfo(..), SymbolTable, insertSymbol)
import AST.AST (Literal(..), IntegerLiteral(..), FloatLiteral(..), BooleanLiteral(..), StringLiteral(..))

-- Prueba unitaria para 'processLiteral'
testSemanticAnalyzerLiteral :: Spec
testSemanticAnalyzerLiteral = describe "processLiteral" $ do
    it "Should process integer literal and update the symbol table correctly" $ do
        let literal = IntLit (IntegerLiteral 42)
            initialTable = mempty
            expectedTable = insertSymbol "intLiteral_42" (SymbolInfo "int" "global" (Just "42")) initialTable
            resultTable = processLiteral literal initialTable
        resultTable `shouldBe` expectedTable

    it "Should process float literal and update the symbol table correctly" $ do
        let literal = FloatLit (FloatLiteral 3.14)
            initialTable = mempty
            expectedTable = insertSymbol "floatLiteral_3.14" (SymbolInfo "float" "global" (Just "3.14")) initialTable
            resultTable = processLiteral literal initialTable
        resultTable `shouldBe` expectedTable

    it "Should process boolean literal and update the symbol table correctly" $ do
        let literal = BoolLit (BooleanLiteral True)
            initialTable = mempty
            expectedTable = insertSymbol "boolLiteral_True" (SymbolInfo "bool" "global" (Just "True")) initialTable
            resultTable = processLiteral literal initialTable
        resultTable `shouldBe` expectedTable

    it "Should process string literal and update the symbol table correctly" $ do
        let literal = StrLit (StringLiteral "hello")
            initialTable = mempty
            expectedTable = insertSymbol "stringLiteral_hello" (SymbolInfo "string" "global" (Just "hello")) initialTable
            resultTable = processLiteral literal initialTable
        resultTable `shouldBe` expectedTable


main :: IO ()
main = hspec testSemanticAnalyzerLiteral