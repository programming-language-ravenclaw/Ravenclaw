module Methods.MethodCallParserTest where
import Test.Hspec
import Text.Parsec
import qualified Expression.ExpressionParser(methodCallParser)
import AST.AST
import Methods.NameMethodParser (nameMethodParser)
import Data.Either (isLeft)

import qualified Data.Text as T
import AST.AST (LiteralExpression(BoolExpr), BooleanExpression (BooleanExprComparison), ComparisonExpression (BooleanComparison))

testMethodCallParser :: Spec
testMethodCallParser = describe "methodCallParser" $ do
    it "parses a method call with no arguments" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "methodName()") `shouldBe`
            Right (MethodCall (Identifier (Letter "methodName") []) [])

    it "parses a method call with one argument" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "add(2)") `shouldBe`
            Right (MethodCall (Identifier (Letter "add") []) [LiteralExpr (IntLit (IntegerLiteral 2))])

    it "parses a method call with multiple arguments" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "print(1, \"Hello\", true)") `shouldBe`
            Right (MethodCall (Identifier (Letter "print") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (StrLit (StringLiteral "Hello")),
                      BooleanExpr (BooleanExprComparison (BooleanComparison (BooleanLiteral True)) [])])

    it "fails to parse method call with missing closing parenthesis" $ do
        let result = parse Expression.ExpressionParser.methodCallParser "" (T.pack "method(")
        result `shouldSatisfy` isLeft

    it "fails to parse method call with invalid argument" $ do
        let result = parse Expression.ExpressionParser.methodCallParser "" (T.pack "method(1, 2,)")
        result `shouldSatisfy` isLeft

    it "parses method call with whitespace around arguments" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "method ( 1 , 2 ) ") `shouldBe`
            Right (MethodCall (Identifier (Letter "method") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (IntLit (IntegerLiteral 2))
                    ])

    it "parses method call with no spaces around arguments" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "method(1,2)") `shouldBe`
            Right (MethodCall (Identifier (Letter "method") []) [
                      LiteralExpr (IntLit (IntegerLiteral 1)),
                      LiteralExpr (IntLit (IntegerLiteral 2))
                    ])

    it "parses method call with empty argument list" $ do
        parse Expression.ExpressionParser.methodCallParser "" (T.pack "method()") `shouldBe`
            Right (MethodCall (Identifier (Letter "method") []) [])