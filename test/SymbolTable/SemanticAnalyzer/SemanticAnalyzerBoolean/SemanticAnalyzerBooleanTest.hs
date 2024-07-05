module SymbolTable.SemanticAnalyzer.SemanticAnalyzerBoolean.SemanticAnalyzerBooleanTest where

import Test.Hspec
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerBoolean.SemanticAnalyzerBoolean (processBooleanExpression)
import SymbolTable.SymbolTable (SymbolInfo(..), SymbolTable, insertSymbol)
import qualified Data.Map as Map
import AST.AST (Comment(..))

-- | Represents an empty symbol table.
-- 
-- This function defines an initial 'SymbolTable' with no symbols.
-- It is implemented using 'Map.empty' from the 'Data.Map' library,
-- providing a starting point for symbol table operations.
emptyTable :: SymbolTable
emptyTable = Map.empty

-- Define tests for the Semantic Analyzer Boolean module
testSemanticAnalyzerBoolean :: Spec
testSemanticAnalyzerBoolean = do
  describe "processBooleanExpression" $ do
    it "evaluates boolean expressions correctly with predefined symbols" $ do
      let boolSymbolInfoTrue = SymbolInfo "bool" "global" (Just "true")
          boolSymbolInfoFalse = SymbolInfo "bool" "global" (Just "false")
          table = insertSymbol "x" boolSymbolInfoTrue emptyTable
      processBooleanExpression table "x && false" `shouldBe` Just False
      processBooleanExpression table "x || false" `shouldBe` Just True
      processBooleanExpression table "true && false" `shouldBe` Just False
      processBooleanExpression table "true || false" `shouldBe` Just True
      processBooleanExpression table "true && true" `shouldBe` Just True
      processBooleanExpression table "false && true" `shouldBe` Just False
      processBooleanExpression table "true && (false || true)" `shouldBe` Just True
    it "returns Nothing for invalid boolean expressions" $ do
      let table = emptyTable
      processBooleanExpression table "x &&" `shouldBe` Nothing
      processBooleanExpression table "true &&" `shouldBe` Nothing
      processBooleanExpression table "&& false" `shouldBe` Nothing
    it "handles complex boolean expressions" $ do
      let boolSymbolInfoFalse = SymbolInfo "bool" "global" (Just "false")
          table = insertSymbol "y" boolSymbolInfoFalse emptyTable
      processBooleanExpression table "y || (true && false)" `shouldBe` Just False
      processBooleanExpression table "y || (true || false)" `shouldBe` Just True
      processBooleanExpression table "(y && false) || (true && true)" `shouldBe` Just True

main :: IO ()
main = hspec testSemanticAnalyzerBoolean
