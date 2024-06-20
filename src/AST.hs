module AST where

data Program = Program [GlobalStatement]
             deriving (Show, Eq)

data GlobalStatement =  Statement Statement
                     deriving (Show, Eq)

data Statement = Comment Comment
               | Literal Literal
               deriving (Show, Eq)

data Comment = LineComment String
             | BlockComment String
             deriving (Eq, Show)

data Literal = IntLit IntegerLiteral
             | FloatLit FloatLiteral
             | BoolLit BooleanLiteral
             | StrLit StringLiteral
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