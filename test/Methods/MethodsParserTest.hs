module Methods.MethodsParserTest where

import Test.Hspec
import Text.Parsec
import Parser (program)
import AST.AST
import qualified Data.Text as T
import Methods.MethodsParser

testParseMethod :: Spec
testParseMethod = describe "parses a method" $ do
    it "parse a method with an int value and a return Expression" $ do
        parse methodDeclarationParser "" (T.pack "method a (int x){return (1+1)}") `shouldBe` Right (MethodDeclaration (NameMethod (Identifier (Letter "a") [])) (ParameterList [DataIden (IntType (DataInt "int")) (Identifier (Letter "x") [])]) [] [ReturnStatementExpression (ArithmeticExpr (IntArithmetic (IntArith (Digit 1) Plus (Digit 1) [])))])
    it "parse a method with a float value and a return Expression" $ do
        parse methodDeclarationParser "" (T.pack "method a (float x){return (1.0+1.0)}") `shouldBe` Right (MethodDeclaration (NameMethod (Identifier (Letter "a") [])) (ParameterList [DataIden (FloatType (DataFloat "float")) (Identifier (Letter "x") [])]) [] [ReturnStatementExpression (ArithmeticExpr (FloatArithmetic (FloatArith (FloatLiteral 1.0) Plus (FloatLiteral 1.0) [])))])
    it "parse a metho with a bool value and a return Expression" $ do
        parse methodDeclarationParser "" (T.pack "method a (bool x){return (true)}") `shouldBe` Right (MethodDeclaration (NameMethod (Identifier (Letter "a") [])) (ParameterList [DataIden (BoolType (DataBool "bool")) (Identifier (Letter "x") [])]) [] [ReturnStatementExpression (BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []))])
    it "parse a method with a string value and a return Expression" $ do
        parse methodDeclarationParser "" (T.pack "method a (str x){return (\"hello\")}") `shouldBe` Right (MethodDeclaration (NameMethod (Identifier (Letter "a") [])) (ParameterList [DataIden (StrType (DataString "str")) (Identifier (Letter "x") [])]) [] [ReturnStatementExpression (LiteralExpr (StrLit (StringLiteral "hello")))])
    it "parse a method with a list value and a return expression" $ do
        parse methodDeclarationParser "" (T.pack "method a (list x){return ([1,2,3])}") `shouldBe` Right (MethodDeclaration (NameMethod (Identifier (Letter "a") [])) (ParameterList [DataIden (ListType (DataList "list")) (Identifier (Letter "x") [])]) [] [ReturnStatementExpression (ListExpression (ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]))])


testsMethodsParser :: Spec
testsMethodsParser = do
    testParseMethod