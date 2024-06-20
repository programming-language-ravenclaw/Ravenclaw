module Lib
    ( someFunc
    ) where

import Text.Parsec (parse)
import qualified Data.Text.IO as TIO
import Parser (literalsParser)

someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "./resources/code.rvc"
    let result = parse literalsParser "./resources/code.rvc" contents
    print result
    