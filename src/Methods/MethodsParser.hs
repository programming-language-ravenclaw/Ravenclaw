module Methods.MethodsParser where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.ExpressionParser (expression)
import Statement.DataTypeDeclarationParser
import Statement.StatementParser (statement)

-- Parser for a NameMethod 
-- This parser expects an identifier and wraps it in a NameMethod
nameMethodParser :: Parser NameMethod
nameMethodParser = NameMethod <$> identifier

-- Parser for a ReturnStatement (e.g. "return (<expression>)" or "return (<literal>)")
-- This parser expects the keyword "return", followed by an expression or literal in parentheses
returnStatementParser :: Parser ReturnStatement
returnStatementParser =
  try (ReturnStatementExpression <$> (string "return" <* spaces *>
                                       char '(' *> spaces *> expression <* spaces <* char ')'))
  <|> try (ReturnStatementLiteral <$> (string "return" *> spaces *>
                                        char '(' *> spaces *> literal <* spaces <* char ')'))
  <?> "expected return statement"

-- Parser for a DataIden
-- This parser expects a data type, followed by an identifier
dataIdenParser :: Parser DataIden
dataIdenParser = DataIden <$> (spaces *> dataType <* spaces) <*> (spaces *> identifier <* spaces)

-- Parser for a ParameterList")
-- This parser expects a list of dataIden, separated by commas, enclosed in parentheses
parameterListParser :: Parser ParameterList
parameterListParser = ParameterList <$> (spaces *> char '(' *> spaces *> dataIdenParser `sepBy1` (spaces *> char ',' <* spaces) <* spaces <* char ')' <* spaces)
  <?> "expected parameter list"

-- Parser for a MethodDeclaration
methodDeclarationParser :: Parser MethodDeclaration
methodDeclarationParser = MethodDeclaration <$> 
  -- Expect the keyword "method", followed by a method name
  (spaces *> string "method" *> spaces *> nameMethodParser <* spaces)
  <*>
  -- Expect a parameter list
  (spaces *> parameterListParser <* spaces)
  <*> 
  -- Expect a block of statements
  (spaces *> char '{' *> spaces *> many statement <* spaces)
  <*> 
  -- Expect return statements
  (spaces *> many returnStatementParser <* spaces <* char '}' <* spaces)
  <?> "expected method declaration"
