-- test/SymbolTable/SemanticAnalyzerIdentifierTest.hs
module SymbolTable.SemanticAnalyzerIdentifierTest where

import Test.Hspec
import SymbolTable.SemanticAnalyzerIdentifier (processIdentifier)
import SymbolTable.SymbolTable (SymbolInfo(..), SymbolTable, insertSymbol)
import AST.AST (Identifier(..), IdentifierPart(..), Letter(..), Digit(..))
import qualified Data.Map as Map

-- Prueba unitaria para 'processIdentifier'
testProcessIdentifier :: Spec
testProcessIdentifier = describe "processIdentifier" $ do
    it "Should process identifier and update the symbol table correctly" $ do
        let identifier = Identifier (Letter "x") [LetterPart (Letter "y"), DigitPart (Digit 1)]
            initialTable = Map.empty
            expectedTable = Map.fromList [("1", SymbolInfo "digit" "global" (Just "1")),
                                          ("y", SymbolInfo "letter" "global" (Just "y")),
                                          ("x", SymbolInfo "identifier" "global" (Just "x"))]
            resultTable = processIdentifier identifier initialTable
        resultTable `shouldBe` expectedTable

main :: IO ()
main = hspec testProcessIdentifier