module Statement.DataTypeDeclarationParserTest where

import Test.Hspec
import Text.Parsec
import Text.Parsec.Text (Parser)
import Statement.DataTypeDeclarationParser
import AST.AST

import qualified Data.Text as T



parserDataTypeDeclarationIntArithTest :: Spec
parserDataTypeDeclarationIntArithTest = describe "parses data type declaration int arith" $ do
    it "parses int" $ do
        parse parserDataTypeDeclarationIntArith "" (T.pack "int x = 1+2") `shouldBe` Right (DataTypeDecIntArith (DataInt "int") (Identifier (Letter "x") []) [IntArith (Digit 1) Plus (Digit 2) []])
    
parserDataTypeDeclarationFloatArithTest :: Spec
parserDataTypeDeclarationFloatArithTest = describe "parses data type declaration float arith" $ do
    it "parses float" $ do
        parse parserDataTypeDeclarationFloatArith "" (T.pack "float x = 1.0+2.0") `shouldBe` Right (DataTypeDecFloatArith (DataFloat "float") (Identifier (Letter "x") []) [FloatArith (FloatLiteral 1.0) Plus (FloatLiteral 2.0) []])

parserDataTypeDeclarationStringArithTest :: Spec
parserDataTypeDeclarationStringArithTest = describe "parses data type declaration string arith" $ do
    it "parses string" $ do
        parse parserDataTypeDeclarationStringArith "" (T.pack "str x = \"hola\"+\"mundo\"") `shouldBe` Right (DataTypeDecStringArith (DataString "str") (Identifier (Letter "x") []) [StringArith (StringLiteral "hola") Concat (StringLiteral "mundo") []])

parserDataTypeDeclarationIntLitTest :: Spec
parserDataTypeDeclarationIntLitTest = describe "parses data type declaration int lit" $ do
    it "parses int" $ do
        parse parserDataTypeDeclarationIntLit "" (T.pack "int x = 1") `shouldBe` Right (DataTypeDecIntLit (DataInt "int") (Identifier (Letter "x") []) [IntegerLiteral 1])

parserDataTypeDeclarationFloatLitTest :: Spec
parserDataTypeDeclarationFloatLitTest = describe "parses data type declaration float lit" $ do
    it "parses float" $ do
        parse parserDataTypeDeclarationFloatLit "" (T.pack "float x = 1.0") `shouldBe` Right (DataTypeDecFloatLit (DataFloat "float") (Identifier (Letter "x") []) [FloatLiteral 1.0])

parserDataTypeDeclarationStringLitTest :: Spec
parserDataTypeDeclarationStringLitTest = describe "parses data type declaration string lit" $ do
    it "parses string" $ do
        parse parserDataTypeDeclarationStringLit "" (T.pack "str x = \"hola\"") `shouldBe` Right (DataTypeDecStringLit (DataString "str") (Identifier (Letter "x") []) [StringLiteral "hola"])        

parserDataTypeDeclarationBoolTest :: Spec
parserDataTypeDeclarationBoolTest = describe "parses data type declaration bool lit" $ do
    it "parses bool" $ do
        parse parserDataTypeDeclarationBool "" (T.pack "bool x = true") `shouldBe` Right (DataTypeDecBool (DataBool "bool") (Identifier (Letter "x") []) [BooleanExprComparison (BooleanComparison (BooleanLiteral True)) []])

parserDataTypeDeclarationListTest :: Spec
parserDataTypeDeclarationListTest = describe "parses data type declaration list" $ do
    it "parses list" $ do
        parse parserDataTypeDeclarationList "" (T.pack "list x = [1,2,3]") `shouldBe` Right (DataTypeDecList (DataList "list") (Identifier (Letter "x") []) [ListExpr [LiteralExpr (IntLit (IntegerLiteral 1)),LiteralExpr (IntLit (IntegerLiteral 2)),LiteralExpr (IntLit (IntegerLiteral 3))]])

testParseDataTypeDeclaration :: Spec
testParseDataTypeDeclaration = do
    parserDataTypeDeclarationIntArithTest
    parserDataTypeDeclarationFloatArithTest
    parserDataTypeDeclarationStringArithTest
    parserDataTypeDeclarationIntLitTest
    parserDataTypeDeclarationFloatLitTest
    parserDataTypeDeclarationStringLitTest
    parserDataTypeDeclarationBoolTest
    parserDataTypeDeclarationListTest