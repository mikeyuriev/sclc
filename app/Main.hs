module Main where

import Control.Monad.State (unless)
import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Parsec.Error
import SmallCalc.Eval
import SmallCalc.Parser

main :: IO ()
main = loop (Right 0.0)

loop :: Either ParseError Float -> IO ()
loop value = do
    putResult value
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ loop $ eval <$> parse line "" input

putResult :: Either ParseError Float -> IO ()
putResult (Right value) = print value
putResult (Left err)    = putStrLn $ "Error " ++ show err

