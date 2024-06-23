module Main where

import Test.Hspec
import TestParser
import Statement.LoopStatement.LoopStatementTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLoop
