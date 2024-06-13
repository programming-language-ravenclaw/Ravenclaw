module Lib
    ( someFunc
    ) where

import Text.Parsec (parse)
import Parser (literalsParser, returnParser)
import qualified Data.Text.IO as TIO



someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "./resources/code.rvc"
    let result = parse returnParser "./resources/code.rvc" contents
    print result