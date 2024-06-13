module Lib
    ( someFunc
    ) where

import Text.Parsec (parse)
import Parser (literalsParser)
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "src/code.rvc"
    let result = parse literalsParser "src/code.rvc" contents
    print result