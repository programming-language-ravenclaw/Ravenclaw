module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Methods.MethodCallParserTest


main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testMethodCallParser
