module Lib
    ( someFunc
    ) where

import Text.Parsec
import qualified Data.Text.IO as T
import Parser (program)
import SymbolTable.SemanticAnalyzer.SemanticAnalyzer (buildSymbolTable)
import qualified Data.Map as Map
import CodeGenerator.CodeGenerator

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
            let pythonCode = generateCode ast symbolTable
            writeFile "./resources/generated_code.py" pythonCode
            print "Generated Python Code written to generated_code.py"
