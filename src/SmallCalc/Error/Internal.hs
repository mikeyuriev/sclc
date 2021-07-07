module SmallCalc.Error.Internal where

import Text.Parsec.Error
import Text.Parsec.Pos
import SmallCalc.Types

fromParseError :: ParseError -> Error
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
        isExpected _          = False

