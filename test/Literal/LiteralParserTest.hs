module Literal.LiteralParserTest where

import Test.Hspec
import Text.Parsec
import Literals.LiteralParser
import AST.AST

import qualified Data.Text as T

testParseIntegerLiteral :: Spec
testParseIntegerLiteral = describe "parses integer literals" $ do
  it "parses positive integers" $ do
    parse literal "" (T.pack "123") `shouldBe` Right (IntLit (IntegerLiteral 123))

  it "parses zero" $ do
    parse literal "" (T.pack "0") `shouldBe` Right (IntLit (IntegerLiteral 0))

testParseFloatLiteral :: Spec
testParseFloatLiteral = describe "parses float literals" $ do
  it "parses float numbers" $ do
    parse literal "" (T.pack "123.46") `shouldBe` Right (FloatLit (FloatLiteral 123.46))

  it "parses float numbers with leading zero" $ do
    parse literal "" (T.pack "0.123") `shouldBe` Right (FloatLit (FloatLiteral 0.123))

testParseBooleanLiteral :: Spec
testParseBooleanLiteral = describe "parses boolean literals" $ do
  it "parses true" $ do
    parse literal "" (T.pack "true") `shouldBe` Right (BoolLit (BooleanLiteral True))

  it "parses false" $ do
    parse literal "" (T.pack "false") `shouldBe` Right (BoolLit (BooleanLiteral False))

testParseStringLiteral :: Spec
testParseStringLiteral = describe "parses string literals" $ do
  it "parses a simple string" $ do
    parse literal "" (T.pack "\"hello\"") `shouldBe` Right (StrLit (StringLiteral "hello"))

  it "parses an empty string" $ do
    parse literal "" (T.pack "\"\"") `shouldBe` Right (StrLit (StringLiteral ""))

testParseIdentifier :: Spec
testParseIdentifier = describe "parses identifiers" $ do
  it "parses a simple identifier" $ do
    parse identifier "" (T.pack "x") `shouldBe` Right (Identifier (Letter "x") [])

  it "parses an identifier with digits" $ do
    parse identifier "" (T.pack "x1") `shouldBe` Right (Identifier (Letter "x") [DigitPart (Digit 1)])

  it "parses a complex identifier" $ do
    parse identifier "" (T.pack "var123") `shouldBe` Right (Identifier (Letter "var") [DigitPart (Digit 123)])

testParseLiteral :: Spec
testParseLiteral = do
  testParseIntegerLiteral
  testParseFloatLiteral
  testParseBooleanLiteral
  testParseStringLiteral
  testParseIdentifier