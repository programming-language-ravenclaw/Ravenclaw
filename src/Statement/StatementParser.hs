module Statement.StatementParser where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Expression.BooleanExpressionParser
import Literals.LiteralParser
import Expression.ExpressionParser
import Data.Maybe
import Statement.DataTypeDeclarationParser
import Statement.CommentParser
import Statement.PrintParser

statement :: Parser Statement
statement = choice
    [ try (spaces *> (LoopStatement <$> loopStatement) <* spaces)
    , try (spaces *> (ConditionalStatement <$> conditionalStatementParser) <* spaces)
    , try (spaces *> (DataTypeDeclarationStatement <$> dataTypeDeclarationParser) <* spaces)
    , try (spaces *> (ExpressionStatement <$> expression) <* spaces)
    , try (spaces *> (LiteralStatement <$> literal) <* spaces)
    , try (spaces *> (Printer <$> printer) <* spaces)
    , try (spaces *> (Comment <$> comment) <* spaces)
    , try (spaces *> (ListStatement <$> listExpression) <* spaces)
    ]

loopStatement :: Parser LoopStatement
loopStatement = try whileLoop <|> try forLoop

whileLoop :: Parser LoopStatement
whileLoop = WhileLoop <$> (string "while" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
              <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

forLoop :: Parser LoopStatement
forLoop = ForLoop <$> (string "for" *> spaces *> char '(' *> spaces *> identifier)
            <*> (spaces *> string "in" *> spaces *> listExpression <* spaces <* char ')')
            <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

conditionalStatementParser :: Parser ConditionalStatment
conditionalStatementParser = booleanExpressionParser

booleanExpressionParser :: Parser ConditionalStatment
booleanExpressionParser = IfStatement
    <$> (string "if" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
    <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
    <*> (spaces *> many diffIfStatementParser)
    <*> (spaces *> (maybeToList <$> optionMaybe elseStatementParser))

diffIfStatementParser :: Parser DiffIfStatement
diffIfStatementParser = DiffIf <$> (string "diffif" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
           <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

elseStatementParser :: Parser ElseStatement
elseStatementParser = Else <$> (string "else" *> spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

