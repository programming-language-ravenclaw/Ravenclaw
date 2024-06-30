module SymbolTable.SymbolTable where

import qualified Data.Map as Map
import Data.Maybe ()

type SymbolTable = Map.Map String SymbolInfo

data SymbolInfo = SymbolInfo
    { symbolType    :: String
    , symbolScope   :: String
    , symbolAddress :: Maybe Int
    , symbolValue   :: Maybe String
    } deriving (Show, Eq)

insertSymbol :: String -> SymbolInfo -> SymbolTable -> SymbolTable
insertSymbol = Map.insert

lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol = Map.lookup

deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol = Map.delete