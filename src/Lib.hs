module Lib
    ( someFunc
    ) where
import System.IO (readFile)
import Text.Parsec (parse)
import Parser (literalsParser)
import AST
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

someFunc :: IO ()
someFunc = do
    contents <- readFile "code.rvc"
    let textContents = T.pack contents
    let result = parse literalsParser "code.rvc" textContents
    print result