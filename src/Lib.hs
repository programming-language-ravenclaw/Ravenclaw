module Lib
    ( someFunc
    ) where

import Text.Parsec (parse)
import Parser (literalsParser)
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "code.rvc"
    let result = parse literalsParser "code.rvc" contents
    print result