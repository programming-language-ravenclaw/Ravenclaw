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

data Operator = Plus 
                | Minus 
                | Multiply 
                | Divide
              deriving (Show, Eq)

data OperatorConcat = Concat
              deriving (Show, Eq)
              
data BooleanOperator = And 
                    | Or
                     deriving (Show, Eq)

data RelationalOperator = Equal 
                        | NotEqual 
                        | LessThan 
                        | GreaterThan 
                        | LessThanOrEqual 
                        | GreaterThanOrEqual
                        deriving (Show, Eq)

data Expression = ArithmeticExpr ArithmeticExpression
                | BooleanExpr BooleanExpression
                deriving (Show, Eq)

data ArithmeticExpression = IntArithmetic IntArithmetic
                          | FloatArithmetic FloatArithmetic
                          | StringArithmetic StringArithmetic
                          | MixedArithmetic MixedArithmetic
                          deriving (Show, Eq)

data IntArithmetic = IntArith Digit Operator Digit [OperatorAndDigit]
                   deriving (Show, Eq)

data OperatorAndDigit = OpAndDigit Operator Digit
                      deriving (Show, Eq)

data FloatArithmetic = FloatArith FloatLiteral Operator FloatLiteral [OperatorAndFloat]
                     deriving (Show, Eq)

data OperatorAndFloat = OpAndFloat Operator FloatLiteral
                      deriving (Show, Eq)

data StringArithmetic = StringArith StringLiteral OperatorConcat StringLiteral [OperatorAndString]
                      deriving (Show, Eq)

data OperatorAndString = OpAndString OperatorConcat StringLiteral
                       deriving (Show, Eq)

data MixedArithmetic = DigitMixed Digit Operator MixedTail [OperatorAndMixed]
                     | FloatMixed FloatLiteral Operator MixedTail [OperatorAndMixed]
                     deriving (Show, Eq)

data MixedTail = DigitTail Digit
               | FloatTail FloatLiteral
               deriving (Show, Eq)

data OperatorAndMixed = OpAndMixed Operator MixedTail deriving (Show, Eq)

data BooleanExpression = BooleanExprComparison ComparisonExpression [BooleanOpAndComparison]
                       deriving (Show, Eq)

data BooleanOpAndComparison = BooleanOpComp BooleanOperator ComparisonExpression
                            deriving (Show, Eq)

data ComparisonExpression = Comparison ArithmeticExpression [RelationalOpAndArith]
                          deriving (Show, Eq)

data RelationalOpAndArith = RelationalOpArith RelationalOperator ArithmeticExpression
                          deriving (Show, Eq)

data Statement = LoopStatement LoopStatement
                | ExpressionStatement Expression
               deriving (Show, Eq)

data LoopStatement = WhileLoop BooleanExpression [Statement]
                   | ForLoop Identifier ListExpression [Statement]
                   deriving (Show, Eq)

data ListExpression = ListExpr [Literal]
                    deriving (Show, Eq)