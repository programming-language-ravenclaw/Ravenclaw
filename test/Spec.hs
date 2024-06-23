module Main where

import Test.Hspec
import Comment.CommentParserTest
import Literal.LiteralParserTest

main :: IO ()
main = hspec $ do
  testParse
  testParseLiteral
