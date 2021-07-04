module Main where

import Control.Monad.State (unless)
import Data.Bool (bool)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Text.Parsec as P
import SmallCalc.Eval
import SmallCalc.Parser
import SmallCalc.Error

main :: IO ()
main = do
    args <- getArgs
    bool (handleArg $ head args) loop (null args)

handleArg :: String -> IO ()
handleArg arg = do
    let result = evalLine "Command line" arg
    putStrLn $ " " ++ arg ++ " = "
    putStr $ showResult result

loop :: IO ()
loop = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        putStr $ showResult $ evalLine "User input" input
        loop

evalLine :: P.SourceName -> String -> Either Error Double
evalLine source s = evalParseResult $ P.parse line source s

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
