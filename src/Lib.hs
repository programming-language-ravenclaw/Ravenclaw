module Lib
    ( someFunc
    ) where

import Text.Parsec (parse, (<|>))
import Parser (literalsParser, instructionsParser)
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    contents <- TIO.readFile "code.rvc"
    let result = parse instructionsParser "code.rvc" contents
    print result