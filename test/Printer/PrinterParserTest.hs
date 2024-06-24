module Printer.PrinterParserTest where

import Test.Hspec
import Parser(program)
import AST.AST
import qualified Data.Text as T
import Text.Parsec

testParsePrinter :: Spec
testParsePrinter = describe "parses printer" $ do
  it "parses printer" $ do
    parse program "" (T.pack "print(123)") `shouldBe` Right (Program [Statement (Printer (Print (LiteralExpr (IntLit (IntegerLiteral 123)))))])

  it "parses block comments" $ do
    parse program "" (T.pack "print(3.14)") `shouldBe` Right (Program [Statement (Printer (Print (LiteralExpr (FloatLit (FloatLiteral 3.14)))))])
    
  it "parses printer" $ do
    parse program "" (T.pack "print(true)") `shouldBe` Right (Program [Statement (Printer (Print (BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []))))])

  it "parses block comments" $ do
    parse program "" (T.pack "print(2 + 3)") `shouldBe` Right (Program [Statement (Printer (Print (ArithmeticExpr (IntArithmetic (IntArith (Digit 2) Plus (Digit 3) [])))))])
 
  it "parses printer" $ do
    parse program "" (T.pack "print(true && false)") `shouldBe` Right (Program [Statement (Printer (Print (BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [BooleanOpComp And (BooleanComparison (BooleanLiteral False))]))))])

  it "parses block comments" $ do
    parse program "" (T.pack "print(\"hello world\")") `shouldBe` Right (Program [Statement (Printer (Print (LiteralExpr (StrLit (StringLiteral "hello world")))))])

  it "parses printer" $ do
    parse program "" (T.pack "print(\"hola mundo\")") `shouldBe` Right (Program [Statement (Printer (Print (LiteralExpr (StrLit (StringLiteral "hola mundo")))))])

testParsePrint :: Spec
testParsePrint = do 
    testParsePrinter