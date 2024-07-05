module Utils.ExtractLiteral (
    extractDigitValue,
    extractFloatValue,
    extractStringValue
) where

import AST.AST

-- | Extracts the integer value from a 'Digit'.
extractDigitValue :: Digit -> Integer
extractDigitValue (Digit digit) = digit

-- | Extracts the float value from a 'FloatLiteral'.
extractFloatValue :: FloatLiteral -> Float
extractFloatValue (FloatLiteral float) = float

-- | Extracts the string value from a 'StringLiteral'.
extractStringValue :: StringLiteral -> String
extractStringValue (StringLiteral string) = string