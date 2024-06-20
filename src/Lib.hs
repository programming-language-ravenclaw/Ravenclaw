module Lib
    ( someFunc
    ) where

import Text.Parsec
import qualified Data.Text.IO as T
import Parser (statementsParser)

someFunc :: IO ()
someFunc = do
    input <- T.readFile "./resources/code.rvc"
    let result = parse statementsParser "" input
    case result of
        Left err -> print err
        Right statements -> print statements