module Lib
    ( someFunc
    ) where

import Text.Parsec
import qualified Data.Text.IO as T
import Parser (program)

someFunc :: IO ()
someFunc = do
    input <- T.readFile "./resources/code.rvc"
    let result = parse program "" input
    case result of
        Left err -> print err
        Right statements -> print statements