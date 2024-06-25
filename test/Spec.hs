module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Statement.ConditionalStatement.ConditionalStatementTest
import Statement.DataDeclaration.DataTypeDeclarationParserTest (testParseDataTypeDeclaration)
import Methods.MethodsParserTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testParseConditional
  testParseDataTypeDeclaration
  testsMethodsParser
