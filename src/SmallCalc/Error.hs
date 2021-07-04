module SmallCalc.Error
    ( Error (..)
    , fromParseError
    ) where

import Text.Parsec.Error
import Text.Parsec.Pos

data Error
    = SyntaxError Int [String]
    | DivisionByZero
    deriving (Eq, Show)

fromParseError :: ParseError -> Error
fromParseError err
    = SyntaxError (sourceColumn $ errorPos err) (expectedTokens err)

expectedTokens :: ParseError -> [String]
expectedTokens
    = filter (not . null)
    . map messageString
    . filter isExpected
    . errorMessages
    where
        isExpected :: Message -> Bool
        isExpected (Expect _) = True
        isExpected _            = False

