module AST where

data Literal = IntLit IntegerLiteral
             | FloatLit FloatLiteral
             | BoolLit BooleanLiteral
             | StrLit StringLiteral
             | PrintLit Literal
             deriving (Show, Eq)

data IntegerLiteral = IntegerLiteral Integer deriving (Show, Eq)

data FloatLiteral = FloatLiteral Float deriving (Show, Eq)

data BooleanLiteral = BooleanLiteral Bool deriving (Show, Eq)

data StringLiteral = StringLiteral String deriving (Show, Eq)

data Digit = Digit Char deriving (Show, Eq)

data Letter = Letter Char deriving (Show, Eq)

data Identifier = Identifier Letter [IdentifierPart]
                deriving (Show, Eq)

data IdentifierPart = LetterPart Letter
                    | DigitPart Digit
                    deriving (Show, Eq)