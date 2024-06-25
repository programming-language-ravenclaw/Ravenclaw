module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Methods.MethodCallParserTest

import Statement.ConditionalStatement.ConditionalStatementTest
import Methods.MethodsParserTest
import Statement.ConditionalStatement.ConditionalStatementTest

import Printer.PrinterParserTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testMethodCallParser
  testParseConditional
  testParsePrint
  testsMethodsParser
  testParseConditional
