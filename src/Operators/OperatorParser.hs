module Operators.OperatorParser (
    operator,
    operatorConcat,
    booleanOperator,
    relationalOperator
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST

operator :: Parser Operator
operator = choice
    [ Plus <$ char '+'
    , Minus <$ char '-'
    , Multiply <$ char '*'
    , Divide <$ char '/'
    ]

operatorConcat :: Parser OperatorConcat
operatorConcat = Concat <$ char '+'

booleanOperator :: Parser BooleanOperator
booleanOperator = choice
    [ And <$ string "&&"
    , Or <$ string "||"
    ]

relationalOperator :: Parser RelationalOperator
relationalOperator = choice
    [ Equal <$ try (string "==")
    , NotEqual <$ try (string "/=")
    , LessThan <$ try (char '<' <* notFollowedBy (char '='))
    , GreaterThan <$ try (char '>' <* notFollowedBy (char '='))
    , LessThanOrEqual <$ try (string "<=")
    , GreaterThanOrEqual <$ try (string ">=")
    ]
