module SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmeticTest where

import Test.Hspec
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmetic
import SymbolTable.SymbolTable
import Utils.ExtractLiteral
import AST.AST

import qualified Data.Map as Map

-- | Represents an empty symbol table.
emptyTable :: SymbolTable
emptyTable = Map.empty

-- Test cases for processIntArithmetic
testProcessIntArithmetic :: Spec
testProcessIntArithmetic = do
  describe "processIntArithmetic" $ do
    it "processes a simple integer arithmetic expression" $ do
      let expr = IntArith (Digit 3) Plus (Digit 4) []
          resultTable = processIntArithmetic expr emptyTable
      Map.lookup "intArithmetic 3 Plus 4" resultTable `shouldBe` Nothing

-- Test cases for processFloatArithmetic
testProcessFloatArithmetic :: Spec
testProcessFloatArithmetic = do
  describe "processFloatArithmetic" $ do
    it "processes a simple float arithmetic expression" $ do
      let expr = FloatArith (FloatLiteral 3.0) Plus (FloatLiteral 4.5) []
          resultTable = processFloatArithmetic expr emptyTable
      Map.lookup "floatArithmetic 3.0 Plus 4.5" resultTable `shouldBe` Nothing

-- Test cases for processStringArithmetic
testProcessStringArithmetic :: Spec
testProcessStringArithmetic = do
  describe "processStringArithmetic" $ do
    it "processes a simple string concatenation" $ do
      let expr = StringArith (StringLiteral "Hello, ") Concat (StringLiteral "world!") []
          resultTable = processStringArithmetic expr emptyTable
      Map.lookup "stringArithmetic \"Hello, \" Concat \"world!\"" resultTable `shouldBe` Nothing
-- Main test suite
testSemanticAnalyzerArithmetic :: Spec
testSemanticAnalyzerArithmetic = do
  testProcessIntArithmetic
  testProcessFloatArithmetic
  testProcessStringArithmetic

main :: IO ()
main = hspec testSemanticAnalyzerArithmetic
