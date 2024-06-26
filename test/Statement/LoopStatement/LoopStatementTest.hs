module Statement.LoopStatement.LoopStatementTest where

import Test.Hspec
import Text.Parsec
import Parser (program)
import AST.AST
import qualified Data.Text as T
import Data.Either

testParseWhileLoop :: Spec
testParseWhileLoop = describe "parses while loops" $ do
  it "parses a simple while loop with no statements" $ do
    parse program "" (T.pack "while (true) {}") `shouldBe` Right (Program [Statement (LoopStatement (WhileLoop (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []) []))])

  it "parses a while loop with one statement" $ do
    parse program "" (T.pack "while (true) { int x = 0 }") `shouldBe` 
      Right (Program [Statement (LoopStatement (WhileLoop (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []) [DataTypeDeclarationStatement (DataTypeDeclarationInt (DataTypeDecIntLit (DataInt "int") (Identifier (Letter "x") []) [IntegerLiteral 0]))]))])
  
  it "parses a while loop with other while" $ do
    parse program "" (T.pack "while ( 13.7 < 23.5 >= 6.7 ) { while (2 + 4 < 5 + 4 && 4 + 9 == 6 + 6) { 5+8 } }") `shouldBe` 
      Right (Program [Statement (LoopStatement (WhileLoop (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 13.7) LessThan (FloatLiteral 23.5) [RelOpFloat GreaterThanOrEqual (FloatLiteral 6.7)])) []) []) [LoopStatement (WhileLoop (BooleanExprComparison (ArithmeticComparison (IntArithmetic (IntArith (Digit 2) Plus (Digit 4) [])) [RelOpArithmetic LessThan (IntArithmetic (IntArith (Digit 5) Plus (Digit 4) []))]) [BooleanOpComp And (ArithmeticComparison (IntArithmetic (IntArith (Digit 4) Plus (Digit 9) [])) [RelOpArithmetic Equal (IntArithmetic (IntArith (Digit 6) Plus (Digit 6) []))])]) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 5) Plus (Digit 8) [])))])]))])
  
  it "parses a while loop with for" $ do
    parse program "" (T.pack "while ( 13.7 < 23.5 >= 6.7 ) { for (num in [1, \"4\", true]) { 5 + 9 } }" ) `shouldBe` 
      Right (Program [Statement (LoopStatement (WhileLoop (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 13.7) LessThan (FloatLiteral 23.5) [RelOpFloat GreaterThanOrEqual (FloatLiteral 6.7)])) []) []) [LoopStatement (ForLoop (Identifier (Letter "num") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (StrLit (StringLiteral "4")),BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [])]) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 5) Plus (Digit 9) [])))])]))])
  
  it "fails to parse a while loop with an empty condition" $ do
    let result = parse program "" (T.pack "while () { }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 8):\nunexpected \")\"\nexpecting white space, digit, \"\\\"\", \"true\" or \"false\""

  it "fails to parse a while loop with without parentheses and braces" $ do
    let result = parse program "" (T.pack "while")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 6):\nunexpected ()\nexpecting white space, \"(\" or letter or digit"


testParseForLoop :: Spec
testParseForLoop = describe "parses for loops" $ do
  it "parses a simple for loop with no statements" $ do
    parse program "" (T.pack "for (x in []) {}") `shouldBe` Right (Program [Statement (LoopStatement (ForLoop (Identifier (Letter "x") []) (ListExpr []) []))])

  it "parses a for loop with one statement" $ do
    parse program "" (T.pack "for (x in [1, 2, 3]) { int y = 1+2 }") `shouldBe`
      Right (Program [Statement (LoopStatement (ForLoop (Identifier (Letter "x") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]) [DataTypeDeclarationStatement (DataTypeDeclarationInt (DataTypeDecIntArith (DataInt "int") (Identifier (Letter "y") []) [IntArith (Digit 1) Plus (Digit 2) []]))]))])

  it "parses a for loop with one statement" $ do
    parse program "" (T.pack "for (num in [1, \"4\", true]) { 54 + 94 for (num in [1, 4, true]) { 5 + 9 } }") `shouldBe`
      Right (Program [Statement (LoopStatement (ForLoop (Identifier (Letter "num") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (StrLit (StringLiteral "4")),BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [])]) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 54) Plus (Digit 94) []))),LoopStatement (ForLoop (Identifier (Letter "num") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 4)),BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [])]) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 5) Plus (Digit 9) [])))])]))])

  it "detects missing 'in' keyword in for loop" $ do
    let result = parse program "" (T.pack "for (num [1, \"4\", true] ){ }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 10):\nunexpected \"[\"\nexpecting space or \"in\""

  it "detects non-list expression in for loop" $ do
    let result = parse program "" (T.pack "for (num in 2, 4, 1 ){ }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 13):\nunexpected \"2\"\nexpecting space, white space or \"[\""

  it "fails to parse a for loop with without parentheses and braces" $ do
    let result = parse program "" (T.pack "for")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 4):\nunexpected ()\nexpecting white space, \"(\" or letter or digit"

testParseLoop :: Spec
testParseLoop = do
  testParseWhileLoop
  testParseForLoop
