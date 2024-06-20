module SemanticAnalyzer where

import AST

data SemanticError = TypeMismatch String | UndefinedVariable String | VariableAlreadyDeclared String deriving (Show, Eq)

type SymbolTable = [(String, DataType)]

data DataType = IntType | FloatType | BoolType | StringType deriving (Show, Eq)

checkVariableDeclaration :: SymbolTable -> (String, DataType) -> Either SemanticError SymbolTable
checkVariableDeclaration table (name, dataType) =
    if name `elem` map fst table
    then Left (VariableAlreadyDeclared $ "Variable " ++ name ++ " already declared")
    else Right ((name, dataType) : table)

checkAssignment :: SymbolTable -> (String, Literal) -> Either SemanticError ()
checkAssignment table (name, literal) = do
    varType <- lookupVariableType name table
    let litType = inferLiteralType literal
    if varType == litType
    then Right ()
    else Left (TypeMismatch $ "Cannot assign " ++ show litType ++ " to " ++ show varType)

lookupVariableType :: String -> SymbolTable -> Either SemanticError DataType
lookupVariableType name table =
    case lookup name table of
        Just dataType -> Right dataType
        Nothing -> Left (UndefinedVariable $ "Variable " ++ name ++ " not declared")

inferLiteralType :: Literal -> DataType
inferLiteralType (IntLit _) = IntType
inferLiteralType (FloatLit _) = FloatType
inferLiteralType (BoolLit _) = BoolType
inferLiteralType (StrLit _) = StringType

checkVariableDeclarations :: SymbolTable -> [(String, DataType)] -> Either SemanticError SymbolTable
checkVariableDeclarations table [] = Right table
checkVariableDeclarations table (decl:decls) = do
    table' <- checkVariableDeclaration table decl
    checkVariableDeclarations table' decls

checkAssignments :: SymbolTable -> [(String, Literal)] -> Either SemanticError ()
checkAssignments _ [] = Right ()
checkAssignments table (assign:assigns) = do
    checkAssignment table assign
    checkAssignments table assigns

semanticAnalysis :: [(String, DataType)] -> [(String, Literal)] -> Either SemanticError ()
semanticAnalysis declarations assignments = do
    table <- checkVariableDeclarations [] declarations
    checkAssignments table assignments
