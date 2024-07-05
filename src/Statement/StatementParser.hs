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
import Utils.ParserUtils (reserved,printerReservedWords)
import Statement.MethodCallParser (methodCallParser)


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
    ,try (spaces *> ifNotReservedWord *> methodCallParser <* spaces)
    ]

-- | Lookahead to ensure we're not in a context where 'methodCall' should be parsed
ifNotReservedWord :: Parser ()
ifNotReservedWord = notFollowedBy $ choice $ map reserved printerReservedWords

-- Parse a loop statement, which can be either a while loop or a for loop
-- This function is used to parse loop statements
loopStatement :: Parser LoopStatement
loopStatement = try whileLoop <|> try forLoop

-- Parse a while loop
-- This function is used to parse while loops
-- e.g: while (2+4<5+4&&4+9==6+6) { 5+8 }
whileLoop :: Parser LoopStatement
whileLoop = WhileLoop <$> (string "while" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
              <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse a for loop
-- This function is used to parse for loops
-- e.g: for (num in [1, "4", true] ){ 5 + 9 } 
forLoop :: Parser LoopStatement
forLoop = ForLoop <$> (string "for" *> spaces *> char '(' *> spaces *> identifier)
            <*> (spaces *> string "in" *> spaces *> listExpression <* spaces <* char ')')
            <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse a conditional statement, which is an if-else statement
-- This function is used to parse conditional statements
-- e.g: if ( 13.8 <= 23   ) { 3 + 5 } 
conditionalStatementParser :: Parser ConditionalStatment
conditionalStatementParser = booleanExpressionParser

-- Parse a boolean expression, which is used in conditional statements
-- This function is used to parse boolean expressions
-- e.g: 13.8 <= 23
booleanExpressionParser :: Parser ConditionalStatment
booleanExpressionParser = IfStatement
    <$> (string "if" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
    <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
    <*> (spaces *> many diffIfStatementParser)
    <*> (spaces *> (maybeToList <$> optionMaybe elseStatementParser))

-- Parse a diff-if statement (i.e., an else-if clause)
-- This function is used to parse diff-if statements
-- e.g: if (13.8 <= 23 ) { 3+5} diffif ( 13.8 <= 23 ) { 3 + 5 }     
diffIfStatementParser :: Parser DiffIfStatement
diffIfStatementParser = DiffIf <$> (string "diffif" *> spaces *> char '(' *> spaces *> booleanExpression <* spaces <* char ')')
           <*> (spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')

-- Parse an else statement
-- This function is used to parse else statements
-- if ( 13.8 <= 23   ) { 3 + 5 } diffif (5 + 8 == 4 + 9) { 7 + 9 } else { 3 + 5 }
elseStatementParser :: Parser ElseStatement
elseStatementParser = Else <$> (string "else" *> spaces *> char '{' *> spaces *> many statement <* spaces <* char '}')
