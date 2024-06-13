module AST where

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



data Expression = ArithExpr ArithmeticExpression
                | BoolExpr BooleanExpression
                deriving (Show, Eq)

data ArithmeticExpression = IntArithmetic [Integer]
                          | FloatArithmetic [Float]
                          | StringArithmetic [String]
                          | MixedArithmetic [Either Integer Float]
                          deriving (Show, Eq)

data BooleanExpression = BooleanExpression [ComparisonExpression] [BooleanOperator]
                       deriving (Show, Eq)

data ComparisonExpression = ComparisonExpression ArithmeticExpression RelationalOperator ArithmeticExpression
                          deriving (Show, Eq)

data RelationalOperator = LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equal | NotEqual
                        deriving (Show, Eq)

data BooleanOperator = And | Or
                     deriving (Show, Eq)

data ReturnStatement = ReturnStatementLiteral (Maybe Literal)  
                    -- | ReturnStatement (Maybe Expression)
                     deriving (Show, Eq)
