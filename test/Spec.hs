module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
<<<<<<< HEAD
import Methods.MethodCallParserTest

=======
import Statement.ConditionalStatement.ConditionalStatementTest
import Methods.MethodsParserTest
>>>>>>> 5d1c956b21e5dd9d1c6414cc9795aefff20a53c9

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
<<<<<<< HEAD
  testMethodCallParser
=======
  testParseConditional
  testsMethodsParser
>>>>>>> 5d1c956b21e5dd9d1c6414cc9795aefff20a53c9
