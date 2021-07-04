module SmallCalc.Error
    ( SmallCalcError (..)
    , fromParseError
    ) where

import Text.Parsec.Error
import Text.Parsec.Pos

data SmallCalcError
    = ESyntax Int [String]
    | EDivisionByZero
    deriving (Eq, Show)

fromParseError :: ParseError -> SmallCalcError
fromParseError err
    = ESyntax (sourceColumn $ errorPos err) (expectedTokens err)

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

