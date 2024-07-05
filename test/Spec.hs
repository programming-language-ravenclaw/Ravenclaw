module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest
import Statement.LoopStatement.LoopStatementTest
import Methods.MethodCallParserTest
import Statement.ConditionalStatement.ConditionalStatementTest
import Methods.MethodsParserTest
import Statement.DataDeclaration.DataTypeDeclarationParserTest
import Printer.PrinterParserTest
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerComments.SemanticAnalyzerCommentsTest
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerArithmetic.SemanticAnalyzerArithmeticTest
import SymbolTable.SemanticAnalyzer.SemanticAnalyzerLiteral.SemanticAnalyzerLiteralTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
  testParseLoop
  testMethodCallParser
  testParseConditional
  testParseDataTypeDeclaration
  testParsePrint
  testsMethodsParser
  testParseConditional
  testSemanticAnalyzerComments
  testSemanticAnalyzerArithmetic
  testSemanticAnalyzerLiteral
