module SmallCalc.Error
    ( expectedTokens
    ) where

import qualified Text.Parsec.Error as E

expectedTokens :: E.ParseError -> [String]
expectedTokens err
    = map E.messageString
    $ filter isExpected
    $ E.errorMessages err
    where
        isExpected :: E.Message -> Bool
        isExpected (E.Expect _) = True
        isExpected _            = False

