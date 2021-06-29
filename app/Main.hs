module Main where

import Control.Monad.State (unless)
import System.IO (hFlush, stdout)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as PP
import SmallCalc.Eval
import SmallCalc.Parser

main :: IO ()
main = loop (Right 0.0)

loop :: Either P.ParseError Float -> IO ()
loop value = do
    putResult value
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ loop $ evalLine "User input" input

putResult :: Either P.ParseError Float -> IO ()
putResult (Right value) = print value
putResult (Left err)    = putStrLn $ "E" ++ graphicErrorPos (errorPos err)

evalLine :: P.SourceName -> String -> Either P.ParseError Float
evalLine source s = eval <$> P.parse line source s

errorPos :: P.ParseError -> Int
errorPos = PP.sourceColumn . P.errorPos

graphicErrorPos :: Int -> String
graphicErrorPos 0 = ""
graphicErrorPos 1 = "^"
graphicErrorPos x = '-' : graphicErrorPos (x - 1)
