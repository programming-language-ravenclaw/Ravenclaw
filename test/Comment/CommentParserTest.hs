module Comment.CommentParserTest where

import Test.Hspec
import Text.Parsec
import Parser (program)
import AST.AST

import qualified Data.Text as T

testParseComment :: Spec
testParseComment = describe "parses comments" $ do
  it "parses line comments" $ do
    parse program "" (T.pack "# This is a comment\n") `shouldBe` Right (Program [Statement (Comment (LineComment " This is a comment"))])

  it "parses block comments" $ do
    parse program "" (T.pack "## Block comment ##") `shouldBe` Right (Program [Statement (Comment (BlockComment " Block comment "))])


testParse :: Spec
testParse = do
  testParseComment
