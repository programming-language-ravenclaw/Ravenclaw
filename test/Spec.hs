module Main where

import Test.Hspec
import TestParser
import Statement.LoopStatement.LoopStatementTest
import Statement.ConditionalStatement.ConditionalStatementTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLoop
  testParseConditional
