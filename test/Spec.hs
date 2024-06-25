module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Statement.ConditionalStatement.ConditionalStatementTest
import Methods.MethodsParserTest
import Statement.DataDeclaration.DataTypeDeclarationParserTest (testParseDataTypeDeclaration)
import Methods.MethodsParserTest

import Printer.PrinterParserTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testParseConditional
  testParseDataTypeDeclaration
  testParsePrint
  testsMethodsParser
  