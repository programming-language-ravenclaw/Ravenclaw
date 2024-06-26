module Statement.ConditionalStatement.ConditionalStatementTest where

import Test.Hspec
import Text.Parsec
import Parser (program)
import AST.AST
import qualified Data.Text as T
import Data.Either

testParseIfStatement :: Spec
testParseIfStatement = describe "parses if statements" $ do
  it "parses simple if statement" $ do
    parse program "" (T.pack "if ( 2 + 6 == 5 + 3 ) { }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (ArithmeticComparison (IntArithmetic (IntArith (Digit 2) Plus (Digit 6) [])) [RelOpArithmetic Equal (IntArithmetic (IntArith (Digit 5) Plus (Digit 3) []))]) []) [] [] []))])

  it "parses an if statement" $ do
    parse program "" (T.pack "if ( 13 <= 23 >= 6 ) { 3 + 5 }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (LiteralComparison (IntExpr (IntegerExpression (IntegerLiteral 13) LessThanOrEqual (IntegerLiteral 23) [RelOpInteger GreaterThanOrEqual (IntegerLiteral 6)])) []) []) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 3) Plus (Digit 5) [])))] [] []))])

  it "parses if-else statement" $ do
    parse program "" (T.pack "if (5 / 8 /= 5*2) { } else { }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (ArithmeticComparison (IntArithmetic (IntArith (Digit 5) Divide (Digit 8) [])) [RelOpArithmetic NotEqual (IntArithmetic (IntArith (Digit 5) Multiply (Digit 2) []))]) []) [] [] [Else []]))])

  it "parses if-else with statements" $ do
    parse program "" (T.pack "if (5 + 4 < 5 * 2) { int x = 243 } else { list x = [3,5,6] }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (ArithmeticComparison (IntArithmetic (IntArith (Digit 5) Plus (Digit 4) [])) [RelOpArithmetic LessThan (IntArithmetic (IntArith (Digit 5) Multiply (Digit 2) []))]) []) [DataTypeDeclarationStatement (DataTypeDeclarationInt (DataTypeDecIntLit (DataInt "int") (Identifier (Letter "x") []) [IntegerLiteral 243]))] [] [Else [DataTypeDeclarationStatement (DataTypeDeclarationList (DataTypeDecList (DataList "list") (Identifier (Letter "x") []) [ListExpr [LiteralExpr (IntLit (IntegerLiteral 3)),LiteralExpr (IntLit (IntegerLiteral 5)),LiteralExpr (IntLit (IntegerLiteral 6))]]))]]))])

  it "parses if-diffif statement" $ do
    parse program "" (T.pack "if (89 >= 12) { } diffif (12.43 < 43.23) { }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (LiteralComparison (IntExpr (IntegerExpression (IntegerLiteral 89) GreaterThanOrEqual (IntegerLiteral 12) [])) []) []) [] [DiffIf (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 12.43) LessThan (FloatLiteral 43.23) [])) []) []) []] []))])

  it "parses if-diffif-else statement" $ do
    parse program "" (T.pack "if (32.43 > 98.43) { } diffif (43.32 >= 32.32) { } else { }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 32.43) GreaterThan (FloatLiteral 98.43) [])) []) []) [] [DiffIf (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 43.32) GreaterThanOrEqual (FloatLiteral 32.32) [])) []) []) []] [Else []]))])

  it "parses if-diffif-else statement with while" $ do
    parse program "" (T.pack "if (32.43 > 98.43) { while (true) { int x = 0 } } diffif (43.32 >= 32.32) { while (true) { float c = 43.89 } } else { while (true) { bool k = true } }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 32.43) GreaterThan (FloatLiteral 98.43) [])) []) []) [LoopStatement (WhileLoop (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []) [DataTypeDeclarationStatement (DataTypeDeclarationInt (DataTypeDecIntLit (DataInt "int") (Identifier (Letter "x") []) [IntegerLiteral 0]))])] [DiffIf (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 43.32) GreaterThanOrEqual (FloatLiteral 32.32) [])) []) []) [LoopStatement (WhileLoop (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []) [DataTypeDeclarationStatement (DataTypeDeclarationFloat (DataTypeDecFloatLit (DataFloat "float") (Identifier (Letter "c") []) [FloatLiteral 43.89]))])]] [Else [LoopStatement (WhileLoop (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []) [DataTypeDeclarationStatement (DataTypeDeclarationBool (DataTypeDecBool (DataBool "bool") (Identifier (Letter "k") []) [BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []]))])]]))])

  it "parses if-diffif-else statement with for" $ do
    parse program "" (T.pack "if (32.43 > 98.43) { for (x in [1, 2, 3]) { int u = 13+232 } } diffif (43.32 >= 32.32) { for (x in [1, 2, 3]) { 1+2 } } else { for (x in [1, 2, 3]) { print(1+2) } }") `shouldBe` 
        Right (Program [Statement (ConditionalStatement (IfStatement (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 32.43) GreaterThan (FloatLiteral 98.43) [])) []) []) [LoopStatement (ForLoop (Identifier (Letter "x") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]) [DataTypeDeclarationStatement (DataTypeDeclarationInt (DataTypeDecIntArith (DataInt "int") (Identifier (Letter "u") []) [IntArith (Digit 13) Plus (Digit 232) []]))])] [DiffIf (BooleanExprComparison (LiteralComparison (FloatExpr (FloatExpression (FloatLiteral 43.32) GreaterThanOrEqual (FloatLiteral 32.32) [])) []) []) [LoopStatement (ForLoop (Identifier (Letter "x") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]) [ExpressionStatement (ArithmeticExpr (IntArithmetic (IntArith (Digit 1) Plus (Digit 2) [])))])]] [Else [LoopStatement (ForLoop (Identifier (Letter "x") []) (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]) [Printer (Print (ArithmeticExpr (IntArithmetic (IntArith (Digit 1) Plus (Digit 2) []))))])]]))])

  it "detects error in if statement with missing condition" $ do
    let result = parse program "" (T.pack "if () { }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 5):\nunexpected \")\"\nexpecting white space, digit, \"\\\"\", \"true\" or \"false\""

  it "detects error in diffif statement without an if before the condition" $ do
    let result = parse program "" (T.pack "diffif () { }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 7):\nunexpected ()\nexpecting letter or digit"

  it "detects error in else statement without an if or if-diffif before the condition" $ do
    let result = parse program "" (T.pack "else{ }")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 5):\nunexpected ()\nexpecting letter or digit"

  it "detects error in if statement with missing body" $ do
    let result = parse program "" (T.pack "if (2>4)")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 9):\nunexpected end of input\nexpecting white space or \"{\""

  it "detects error in else statement with missing body" $ do
    let result = parse program "" (T.pack "if (2>4) {} else")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 17):\nunexpected end of input\nexpecting white space or \"{\""

  it "detects error in diffid statement with missing body" $ do
    let result = parse program "" (T.pack "if (2>4) {} diffif(34 >= 42)")
    result `shouldSatisfy` isLeft
    show result `shouldBe` "Left (line 1, column 29):\nunexpected end of input\nexpecting white space or \"{\""

testParseConditional :: Spec
testParseConditional = do
  testParseIfStatement