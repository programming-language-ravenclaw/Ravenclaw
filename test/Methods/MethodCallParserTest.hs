module Methods.MethodCallParserTest where

import Test.Hspec
import Text.Parsec
import AST.AST
import qualified Data.Text as T
import Parser (program)
import Statement.MethodCallParser (methodCallParser)
import Data.Either (isLeft)

testMethodCallParser :: Spec
testMethodCallParser = describe "methodCallParser" $ do
    it "parses a method call with no arguments" $ do
        parse methodCallParser "" (T.pack "methodName()") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "methodName") []) []))

    it "parses a method call with one argument" $ do
        parse methodCallParser "" (T.pack "add(2)") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "add") []) [LiteralExpr (IntLit (IntegerLiteral 2))]))

    it "parses a method call with multiple arguments" $ do
        parse methodCallParser "" (T.pack "method(1, \"Hello\", true)") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "method") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (StrLit (StringLiteral "Hello")),
                      BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [])]))

    it "fails to parse method call with missing closing parenthesis" $ do
        let result = parse methodCallParser "" (T.pack "method(")
        result `shouldSatisfy` isLeft

    it "fails to parse method call with invalid argument" $ do
        let result = parse methodCallParser "" (T.pack "method(1, 2,)")
        result `shouldSatisfy` isLeft

    it "parses method call with whitespace around arguments" $ do
        parse methodCallParser "" (T.pack "method ( 1 , 2 ) ") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "method") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (IntLit (IntegerLiteral 2))
                    ]))

    it "parses method call with no spaces around arguments" $ do
        parse methodCallParser "" (T.pack "method(1,2)") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "method") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (IntLit (IntegerLiteral 2))
                    ]))

    it "parses method call with empty argument list" $ do
        parse methodCallParser "" (T.pack "method()") `shouldBe`
            Right (MethodCallStatement (MethodCall (Identifier (Letter "method") []) []))
