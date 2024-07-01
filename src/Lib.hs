module Lib
    ( someFunc
    ) where

import Text.Parsec
import qualified Data.Text.IO as T
import Parser (program)
import SymbolTable.SemanticAnalyzer (buildSymbolTable)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = do
    input <- T.readFile "./resources/code.rvc"
    let result = parse program "" input
    case result of
        Left err -> print err
        Right ast -> do
            print "AST"
            print ast
            let symbolTable = buildSymbolTable ast Map.empty
            print "Symbol Table"
            print symbolTable
