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

-- The statement parser, tries to match one of the following parsers
-- This function is the entry point for parsing a statement
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

-- Parse a loop statement, which can be either a while loop or a for loop
-- This function is used to parse loop statements
loopStatement :: Parser LoopStatement
loopStatement = try whileLoop <|> try forLoop

-- Parse a while loop
-- This function is used to parse while loops
whileLoop :: Parser LoopStatement
whileLoop = WhileLoop <$> (string "while" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
              <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse a for loop
-- This function is used to parse for loops
forLoop :: Parser LoopStatement
forLoop = ForLoop <$> (string "for" *> spaces *> char '(' *> spaces *> identifier)
            <*> (spaces *> string "in" *> spaces *> listExpression <* spaces <* char ')')
            <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse a conditional statement, which is an if-else statement
-- This function is used to parse conditional statements
conditionalStatementParser :: Parser ConditionalStatment
conditionalStatementParser = booleanExpressionParser

-- Parse a boolean expression, which is used in conditional statements
-- This function is used to parse boolean expressions
booleanExpressionParser :: Parser ConditionalStatment
booleanExpressionParser = IfStatement
    <$> (string "if" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
    <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
    <*> (spaces *> many diffIfStatementParser)
    <*> (spaces *> (maybeToList <$> optionMaybe elseStatementParser))

-- Parse a diff-if statement (i.e., an else-if clause)
-- This function is used to parse diff-if statements
diffIfStatementParser :: Parser DiffIfStatement
diffIfStatementParser = DiffIf <$> (string "diffif" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
           <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse an else statement
-- This function is used to parse else statements
elseStatementParser :: Parser ElseStatement
elseStatementParser = Else <$> (string "else" *> spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
