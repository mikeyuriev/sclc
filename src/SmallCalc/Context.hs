module SmallCalc.Context
    ( getSymbol
    , putSymbol
    ) where

import SmallCalc.Types

getSymbol :: Context -> Name -> Either Error Number
getSymbol (Context syms _) name
    = maybe (Left (EUndefined name)) pure (lookup name syms)

putSymbol :: Context -> Name -> Number -> Context
putSymbol (Context syms _) name number
    = Context (put syms name number) number

put :: Symbols -> Name -> Number -> Symbols
put [] name number = [(name, number)]
put ((name', number') : rest) name number
    | name == name' = (name, number) : rest
    | otherwise     = (name', number') : put rest name number
