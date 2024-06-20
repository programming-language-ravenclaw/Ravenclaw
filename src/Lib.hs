module Lib
    ( someFunc
    ) where

import Text.Parsec (parse)
import Parser (literalsParser,printer)
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "code.rvc"
    let result = parse printer "code.rvc" contents
    print result