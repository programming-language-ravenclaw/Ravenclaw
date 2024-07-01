module Operators.OperatorParser (
    operator,
    operatorConcat,
    booleanOperator,
    relationalOperator
) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import AST.AST

-- | The 'operator' parser recognizes basic arithmetic operators.
--
-- It parses one of the following characters:
--   - '+' for addition, which maps to the 'Plus' constructor.
--   - '-' for subtraction, which maps to the 'Minus' constructor.
--   - '*' for multiplication, which maps to the 'Multiply' constructor.
--   - '/' for division, which maps to the 'Divide' constructor.
operator :: Parser Operator
operator = choice
    [ Plus <$ char '+'
    , Minus <$ char '-'
    , Multiply <$ char '*'
    , Divide <$ char '/'
    ]

-- | The 'operatorConcat' parser recognizes the concatenation operator.
--
-- It parses the '+' character and maps it to the 'Concat' constructor.
operatorConcat :: Parser OperatorConcat
operatorConcat = Concat <$ char '+'

-- | The 'booleanOperator' parser recognizes boolean operators.
--
-- It parses one of the following strings:
--   - "&&" for logical AND, which maps to the 'And' constructor.
--   - "||" for logical OR, which maps to the 'Or' constructor.
booleanOperator :: Parser BooleanOperator
booleanOperator = choice
    [ And <$ string "&&"
    , Or <$ string "||"
    ]

-- | The 'relationalOperator' parser recognizes relational operators.
--
-- It parses one of the following strings or characters:
--   - "==" for equality, which maps to the 'Equal' constructor.
--   - "/=" for inequality, which maps to the 'NotEqual' constructor.
--   - '<' for less than (not followed by '='), which maps to the 'LessThan' constructor.
--   - '>' for greater than (not followed by '='), which maps to the 'GreaterThan' constructor.
--   - "<=" for less than or equal to, which maps to the 'LessThanOrEqual' constructor.
--   - ">=" for greater than or equal to, which maps to the 'GreaterThanOrEqual' constructor.
relationalOperator :: Parser RelationalOperator
relationalOperator = choice
    [ Equal <$ try (string "==")
    , NotEqual <$ try (string "/=")
    , LessThan <$ try (char '<' <* notFollowedBy (char '='))
    , GreaterThan <$ try (char '>' <* notFollowedBy (char '='))
    , LessThanOrEqual <$ try (string "<=")
    , GreaterThanOrEqual <$ try (string ">=")
    ]
