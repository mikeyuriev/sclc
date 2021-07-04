module SmallCalc.Error
    ( Error (..)
    , fromParseError
    ) where

import qualified Text.Parsec.Error as E
import qualified Text.Parsec.Pos as P

data Error
    = SyntaxError Int [String]
    | DivisionByZero
    deriving (Eq, Show)

fromParseError :: E.ParseError -> Error
fromParseError err
    = SyntaxError (P.sourceColumn $ E.errorPos err) (expectedTokens err)

expectedTokens :: E.ParseError -> [String]
expectedTokens err
    = filter (not . null)
    $ map E.messageString
    $ filter isExpected
    $ E.errorMessages err
    where
        isExpected :: E.Message -> Bool
        isExpected (E.Expect _) = True
        isExpected _            = False

