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

data ErrorShowing
    = Short
    | Long
    deriving Eq

main :: IO ()
main = do
    args <- getArgs
    bool (handleArgs args) loop (null args)

handleArgs :: [String] -> IO ()
handleArgs = putStrLn . intercalate "\n" . map (parseAndShow Short)

loop :: IO ()
loop = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        putStrLn . parseAndShow Long $ input
        loop

parseAndShow :: ErrorShowing -> String -> String
parseAndShow showing = showResult showing . evalParseResult . parseLine

showResult :: ErrorShowing -> EvalResult -> String
showResult showing = either (showError showing) (printf "%f")

showError :: ErrorShowing -> SmallCalcError -> String
showError Long (ESyntax pos expected)
    =  "!"
    ++ showGraphicErrorPos pos
    ++ showLst "Expected" expected
showError _ (ESyntax pos _)
    =  "Syntax error at position " ++ show pos
showError _ EDivisionByZero
    =  "Division by zero"

showGraphicErrorPos :: Int -> String
showGraphicErrorPos 0 = ""
showGraphicErrorPos 1 = "^"
showGraphicErrorPos x = '-' : showGraphicErrorPos (x - 1)

showLst :: String -> [String] -> String
showLst _ []         =  ""
showLst header items = "\n" ++ header ++ " " ++ intercalate ", " items
