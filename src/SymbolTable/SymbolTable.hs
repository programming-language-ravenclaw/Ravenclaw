module SymbolTable.SymbolTable where

import qualified Data.Map as Map
import Data.Maybe ()

-- | The 'SymbolTable' type represents a mapping from strings to 'SymbolInfo' values.
-- It is implemented as a 'Map.Map' from the Data.Map library.
type SymbolTable = Map.Map String SymbolInfo

-- | The 'SymbolInfo' data type stores information about a symbol.
-- It contains the following fields:
--   - 'symbolType': A 'String' representing the type of the symbol (e.g., "int", "float", "bool", "string").
--   - 'symbolScope': A 'String' representing the scope of the symbol (e.g., "global", "local").
--   - 'symbolValue': A 'Maybe String' representing the value of the symbol, if any.
data SymbolInfo = SymbolInfo
    { symbolType    :: String
    , symbolScope   :: String
    , symbolValue   :: Maybe String
    } deriving (Show, Eq)

-- | Inserts a symbol into the symbol table.
-- 
-- Takes the following arguments:
--   - 'String': The key for the symbol in the table.
--   - 'SymbolInfo': The information about the symbol to insert.
--   - 'SymbolTable': The symbol table to insert into.
-- 
-- Returns an updated 'SymbolTable' with the new symbol inserted.
insertSymbol :: String -> SymbolInfo -> SymbolTable -> SymbolTable
insertSymbol = Map.insert

-- | Looks up a symbol in the symbol table.
-- 
-- Takes the following arguments:
--   - 'String': The key for the symbol to look up.
--   - 'SymbolTable': The symbol table to search.
-- 
-- Returns a 'Maybe SymbolInfo' containing the symbol information if found, or 'Nothing' if not found.
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol = Map.lookup

-- | Deletes a symbol from the symbol table.
-- 
-- Takes the following arguments:
--   - 'String': The key for the symbol to delete.
--   - 'SymbolTable': The symbol table to delete from.
-- 
-- Returns an updated 'SymbolTable' with the symbol removed.
deleteSymbol :: String -> SymbolTable -> SymbolTable
deleteSymbol = Map.delete