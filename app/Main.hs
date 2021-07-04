module Main where

import Control.Monad.State (unless)
import Data.Bool (bool)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import SmallCalc.Eval
import SmallCalc.Parser
import SmallCalc.Error

main :: IO ()
main = do
    args <- getArgs
    bool (handleArg $ head args) loop (null args)

handleArg :: String -> IO ()
handleArg arg = do
    putStrLn $ " " ++ arg ++ " = "
    putStr $ parseAndShow arg

loop :: IO ()
loop = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        putStr $ parseAndShow input
        loop

parseAndShow :: String -> String
parseAndShow = showResult . evalParseResult . parseLine

showResult :: EvalResult -> String
showResult = either showError (printf "%f\n")

showError :: Error -> String
showError (SyntaxError pos expected)
    =  "!"
    ++ showGraphicErrorPos pos
    ++ showLst "Expected" expected
    ++ "\n"
showError DivisionByZero
    =  "Division by zero\n"

showGraphicErrorPos :: Int -> String
showGraphicErrorPos 0 = ""
showGraphicErrorPos 1 = "^"
showGraphicErrorPos x = '-' : showGraphicErrorPos (x - 1)

showLst :: String -> [String] -> String
showLst _ []         =  ""
showLst header items = "\n" ++ header ++ " " ++ intercalate ", " items
