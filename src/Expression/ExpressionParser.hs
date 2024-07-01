module Expression.ExpressionParser (
    expression,
    listExpression,
    methodCallParser
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST
import Literals.LiteralParser
import Expression.BooleanExpressionParser
import Expression.ArithmeticExpressionParser
import Utils.ParserUtils (whitespace,reserved)
import Methods.NameMethodParser (nameMethodParser)

-- Reserved word declarated in BNF for methodCall purposes
printerReservedWords :: [String]
printerReservedWords = [
    "if", "diffif", "else", "while", "for", "in", "method", "return",
    "int", "float", "bool", "str", "list", "true", "false", "print"
    ]

-- | Parser for an expression that can be of various types.
--
--   This function attempts to parse an expression as:
--
--   * An arithmetic expression
--   * A boolean expression
--   * A literal expression
--   * A list of expressions
--   * A method call expression
--
--   Returns: Parsed 'Expression'.
expression :: Parser Expression
expression = try (ArithmeticExpr <$> arithmeticExpression)
          <|>  (BooleanExpr <$> booleanExpression)
          <|>  (LiteralExpr <$> literal)
          <|>  (ListExpression <$> listExpression)
          <|>  (ifNotReservedWord *> (MethodCallExpr <$> methodCallParser))

-- | Parser for a list expression enclosed in square brackets.
--
--   Parses expressions separated by commas inside square brackets.
--
--   Returns: Parsed 'ListExpression'.
listExpression :: Parser ListExpression
listExpression = ListExpr <$> (spaces *> char '[' *> spaces *> expression `sepBy` (spaces *> char ',' <* spaces) <* spaces <* char ']' <* spaces)

-- | Lookahead to ensure we're not in a context where 'methodCall' should be parsed
ifNotReservedWord :: Parser ()
ifNotReservedWord = notFollowedBy $ choice $ map reserved printerReservedWords

-- | Parser for a method call.
--
--   Parses a method call consisting of a method name followed by a list of arguments.
--
--   Returns: Parsed 'MethodCall'.
methodCallParser :: Parser MethodCall
methodCallParser = MethodCall <$> (extractIdentifier <$> (whitespace *> nameMethodParser <* whitespace)) <*> argumentsParser

-- | Extracts the Identifier from a NameMethod
extractIdentifier :: NameMethod -> Identifier
extractIdentifier (NameMethod ident) = ident

-- | Parser for an expression argument.
--
--   Returns: Parsed 'Expression'.
argumentParser :: Parser Expression
argumentParser = whitespace *> expression <* whitespace

-- | Parser for a list of arguments separated by commas and enclosed in parentheses.
--
--   Returns: List of parsed 'Expression's.
argumentsParser :: Parser [Expression]
argumentsParser = whitespace *> char '(' *> whitespace *> argumentParser `sepBy` (char ',' *> whitespace) <* char ')' <* whitespace
