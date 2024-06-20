module TestParser where

import Test.Hspec
import Text.Parsec
import Parser (literalsParser)
import AST

import qualified Data.Text as T

testParseComment :: Spec
testParseComment = describe "parses comments" $ do
  it "parses line comments" $ do
    parse literalsParser "" (T.pack "# This is a comment\n") `shouldBe` Right [Comment (LineComment " This is a comment")]

  it "parses block comments" $ do
    parse literalsParser "" (T.pack "## Block comment ##") `shouldBe` Right [Comment (BlockComment " Block comment ")]


testParse :: Spec
testParse = do
  testParseComment
