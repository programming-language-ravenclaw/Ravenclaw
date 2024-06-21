module Main where

import Test.Hspec
import TestParser

main :: IO ()
main = hspec $ do
  testParse
