module Main where

import Control.Monad.State (unless)
import Data.List (intercalate)
import System.IO (hFlush, stdout)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as PP
import SmallCalc.Eval
import SmallCalc.Parser
import SmallCalc.Error

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
putResult (Left err)    = do
    putStrLn $ "E" ++ showGraphicErrorPos (errorPos err)
    putList "Expected one of:" $ filter (/= "spaces") $ expectedTokens err

evalLine :: P.SourceName -> String -> Either P.ParseError Float
evalLine source s = eval <$> P.parse line source s

errorPos :: P.ParseError -> Int
errorPos = PP.sourceColumn . P.errorPos

showGraphicErrorPos :: Int -> String
showGraphicErrorPos 0 = ""
showGraphicErrorPos 1 = "^"
showGraphicErrorPos x = '-' : showGraphicErrorPos (x - 1)

putList :: String -> [String] -> IO ()
putList _ []         = return ()
putList header items = do
        putStrLn header
        putStrLn $ intercalate "\n" $ map ("  " ++) items
