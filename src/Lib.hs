{- module Lib
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
        Right statements -> print statements -}

module Lib
    ( someFunc
    ) where

import Text.Parsec
import qualified Data.Text.IO as T
import Parser (program)
import AST.AST (Program)
import SymbolTable.SemanticAnalyzer (buildSymbolTable)
import SymbolTable.SymbolTable (SymbolTable)
import qualified Data.Map as Map

someFunc :: IO ()
someFunc = do
    input <- T.readFile "./resources/code.rvc"
    let result = parse program "" input
    case result of
        Left err -> print err
        Right ast -> do
            -- print ast
            let symbolTable = buildSymbolTable ast Map.empty
            print symbolTable
