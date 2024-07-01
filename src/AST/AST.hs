module AST.AST where

data Program = Program [GlobalStatement] deriving (Show, Eq)

data GlobalStatement =  Statement Statement
                        | Method_Declaration MethodDeclaration
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

data Digit = Digit Integer deriving (Show, Eq)

data Letter = Letter String deriving (Show, Eq)

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
                | LiteralExpr Literal
                | ListExpression ListExpression
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

data MixedArithmetic = DigitMixed Digit Operator FloatLiteral [OperatorAndMixed]
                     | FloatMixed FloatLiteral Operator Digit [OperatorAndMixed]
                     deriving (Show, Eq)

data OperatorAndMixed = OpAndMixedDigit Operator Digit 
                        | OpAndMixedFloat Operator FloatLiteral 
                        deriving (Show, Eq)

data BooleanExpression = BooleanExprComparison ComparisonExpression [BooleanOpAndComparison]
                       deriving (Show, Eq)

data BooleanOpAndComparison = BooleanOpComp BooleanOperator ComparisonExpression
                            deriving (Show, Eq)

data ComparisonExpression = LiteralComparison LiteralExpression [RelationalOpAndLiteral]
                          | ArithmeticComparison ArithmeticExpression [RelationalOpAndArithmetic]
                          | BooleanComparison BooleanLiteral
                          deriving (Show, Eq)

data RelationalOpAndLiteral = RelOpLiteral RelationalOperator LiteralExpression
                            deriving (Show, Eq)

data RelationalOpAndArithmetic = RelOpArithmetic RelationalOperator ArithmeticExpression
                               deriving (Show, Eq)

data LiteralExpression = IntExpr IntegerExpression
                       | FloatExpr FloatExpression
                       | StrExpr StringExpression
                       | MixedExpr MixedExpression
                       | BoolExpr BooleanExpressionLiteral
                       deriving (Show, Eq)

data IntegerExpression = IntegerExpression IntegerLiteral RelationalOperator IntegerLiteral [RelationalOpAndInteger]
                       deriving (Show, Eq)

data RelationalOpAndInteger = RelOpInteger RelationalOperator IntegerLiteral
                            deriving (Show, Eq)

data FloatExpression = FloatExpression FloatLiteral RelationalOperator FloatLiteral [RelationalOpAndFloat]
                     deriving (Show, Eq)

data RelationalOpAndFloat = RelOpFloat RelationalOperator FloatLiteral
                          deriving (Show, Eq)

data StringExpression = StringExpression StringLiteral RelationalOperator StringLiteral [RelationalOpAndString]
                      deriving (Show, Eq)

data RelationalOpAndString = RelOpString RelationalOperator StringLiteral
                           deriving (Show, Eq)

data MixedExpression = MixedExpressionInteger IntegerLiteral RelationalOperator FloatLiteral [RelationalOpAndMixedLiteral]
                      | MixedExpressionFloat FloatLiteral RelationalOperator IntegerLiteral [RelationalOpAndMixedLiteral]
                      deriving(Show, Eq)

data RelationalOpAndMixedLiteral = RelOpMixedLiteralInteger RelationalOperator IntegerLiteral
                                  | RelOpMixedLiteralFloat RelationalOperator FloatLiteral
                                 deriving (Show, Eq)

data BooleanExpressionLiteral = BooleanExpressionLiteral BooleanLiteral RelationalOperator BooleanLiteral [RelationalOpAndBoolean]
                              deriving (Show, Eq)

data RelationalOpAndBoolean = RelOpBoolean RelationalOperator BooleanLiteral
                            deriving (Show, Eq)

data Statement = LoopStatement LoopStatement
                | ExpressionStatement Expression
                | LiteralStatement Literal
                | ConditionalStatement ConditionalStatment
                | Printer Printer
                | DataTypeDeclarationStatement DataTypeDeclaration
                | Comment Comment
                | ListStatement ListExpression
               deriving (Show, Eq)

data ConditionalStatment = IfStatement BooleanExpression [Statement] [DiffIfStatement] [ElseStatement] deriving (Show, Eq)

data DiffIfStatement = DiffIf BooleanExpression [Statement] deriving(Show, Eq)

data ElseStatement = Else [Statement] deriving(Show, Eq)

data LoopStatement = WhileLoop BooleanExpression [Statement]
                   | ForLoop Identifier ListExpression [Statement]
                   deriving (Show, Eq)

data ListExpression = ListExpr [Expression]
                    deriving (Show, Eq)

data Printer = Print Expression
            deriving (Show, Eq)

data DataTypeDeclaration = DataTypeDeclarationInt DataTypeDeclarationInt
                        | DataTypeDeclarationFloat DataTypeDeclarationFloat
                        | DataTypeDeclarationString DataTypeDeclarationString
                        | DataTypeDeclarationBool DataTypeDeclarationBool
                        | DataTypeDeclarationList DataTypeDeclarationList
                        deriving (Show, Eq)

data DataTypeDeclarationInt = DataTypeDecIntArith DataTypeInt Identifier [IntArithmetic]
                            | DataTypeDecIntLit DataTypeInt Identifier [IntegerLiteral]
                            deriving (Show, Eq)

data DataTypeDeclarationFloat = DataTypeDecFloatArith DataTypeFloat Identifier  [FloatArithmetic]
                              | DataTypeDecFloatLit DataTypeFloat Identifier [FloatLiteral]
                            deriving (Show, Eq)                 
data DataTypeDeclarationBool = DataTypeDecBool DataTypeBool Identifier [BooleanExpression]
                            deriving (Show, Eq)
data DataTypeDeclarationString = DataTypeDecStringArith DataTypeString Identifier  [StringArithmetic]
                            | DataTypeDecStringLit DataTypeString Identifier [StringLiteral]
                            deriving (Show, Eq)
data DataTypeDeclarationList = DataTypeDecList DataTypeList Identifier [ListExpression]
                            deriving (Show, Eq)

data DataType =  IntType DataTypeInt
               | FloatType DataTypeFloat
               | BoolType DataTypeBool
               | StrType DataTypeString
               | ListType DataTypeList
               deriving (Show, Eq)

data DataTypeInt = DataInt String
                  deriving (Show, Eq)

data DataTypeFloat = DataFloat String
                  deriving (Show, Eq)

data DataTypeString = DataString String
                  deriving (Show, Eq)

data DataTypeList = DataList String
                  deriving (Show, Eq)

data DataTypeBool = DataBool String
                  deriving (Show, Eq)

data NameMethod = NameMethod Identifier
                  deriving (Show, Eq)

data ReturnStatement = ReturnStatementExpression Expression
                      | ReturnStatementLiteral Literal
                      deriving (Show, Eq)

data ParameterList = ParameterList [DataIden]
                    deriving (Show, Eq)

data DataIden = DataIden DataType Identifier
                    deriving (Show, Eq)                    

data MethodDeclaration = MethodDeclaration NameMethod ParameterList [Statement] [ReturnStatement]
                        deriving (Show, Eq)

