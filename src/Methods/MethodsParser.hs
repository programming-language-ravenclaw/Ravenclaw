module Methods.MethodsParser where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.ExpressionParser (expression)
import Statement.DataTypeDeclarationParser
import Statement.StatementParser (statement)
import Methods.NameMethodParser (nameMethodParser)

returnStatementParser :: Parser ReturnStatement
returnStatementParser = ReturnStatementExpression <$> (string "return" <* spaces *>
                                                char '(' *> spaces *> expression <* spaces <* char ')')
                                            <|> ReturnStatementLiteral <$> (string "return" *> spaces *>
                                                char '(' *> spaces *> literal <* spaces <* char ')')

dataIdenParser :: Parser DataIden
dataIdenParser = DataIden <$> (spaces *> dataType <* spaces) <*> (spaces *> identifier <* spaces)

parameterListParser :: Parser ParameterList
parameterListParser = ParameterList <$> (spaces *> char '(' *> spaces *> dataIdenParser `sepBy1` (spaces *> char ',' <* spaces) <* spaces <* char ')' <* spaces)

methodDeclarationParser :: Parser MethodDeclaration
methodDeclarationParser = MethodDeclaration <$> (spaces *> string "method" *> spaces *> nameMethodParser <* spaces)
                                            <*> (spaces *> parameterListParser <* spaces)
                                            <*> (spaces *> char '{' *> spaces *> many statement <* spaces)
                                            <*> (spaces *> many returnStatementParser <* spaces <* char '}' <* spaces)
