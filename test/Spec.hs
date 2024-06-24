module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Statement.ConditionalStatement.ConditionalStatementTest

import Printer.PrinterParserTest
main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testParseConditional
