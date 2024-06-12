module Lib
    ( someFunc
    ) where
import System.IO (readFile)
import Text.Parsec.String (parseFromFile)
import Parser
import AST

someFunc :: IO ()
someFunc = do
    result <- parseFile "code.rvc"
    case result of
        Left err -> print err
        Right literals -> print literals
