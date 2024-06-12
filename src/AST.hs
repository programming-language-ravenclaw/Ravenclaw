module AST where

data Literal = IntLiteral Integer
             | FloatLiteral Float
             | BoolLiteral Bool
             | StringLiteral String
             deriving (Show, Eq)

data Digit = Digit Char deriving (Show, Eq)

data Letter = Letter Char deriving (Show, Eq)

data Identifier = Identifier Letter [IdentifierPart]
                deriving (Show, Eq)

data IdentifierPart = LetterPart Letter
                    | DigitPart Digit
                    deriving (Show, Eq)