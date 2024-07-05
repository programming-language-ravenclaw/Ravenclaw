module SymbolTable.SemanticAnalyzer.SemanticAnalyzerComments.SemanticAnalyzerCommentsTest where

import Test.Hspec
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerComments.SemanticAnalyzerComments (processComment)
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

testSemanticAnalyzerComments :: Spec
testSemanticAnalyzerComments = do
  describe "processComment" $ do
    it "processes a line comment correctly" $ do
      let comment = LineComment "This is a line comment"
          table = emptyTable
          expectedSymbolInfo = SymbolInfo "line_comment" (Just "This is a line comment")
          expectedTable = insertSymbol ("lineComment_This is a line comment") expectedSymbolInfo table
      processComment comment table `shouldBe` expectedTable

    it "processes a block comment correctly" $ do
      let comment = BlockComment "This is a block comment"
          table = emptyTable
          expectedSymbolInfo = SymbolInfo "block_comment" (Just "This is a block comment")
          expectedTable = insertSymbol ("blockComment_This is a block comment") expectedSymbolInfo table
      processComment comment table `shouldBe` expectedTable

    it "handles empty line comment" $ do
      let comment = LineComment ""
          table = emptyTable
          expectedSymbolInfo = SymbolInfo "line_comment" (Just "")
          expectedTable = insertSymbol ("lineComment_") expectedSymbolInfo table
      processComment comment table `shouldBe` expectedTable

    it "handles empty block comment" $ do
      let comment = BlockComment ""
          table = emptyTable
          expectedSymbolInfo = SymbolInfo "block_comment" (Just "")
          expectedTable = insertSymbol ("blockComment_") expectedSymbolInfo table
      processComment comment table `shouldBe` expectedTable

main :: IO ()
main = hspec testSemanticAnalyzerComments
