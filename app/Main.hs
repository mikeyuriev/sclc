module Main where

import Control.Monad.State (unless, when)
import Data.Bool (bool)
import Data.Either (isLeft)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as PP
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
    when (isLeft result) $ do putStrLn $ " " ++ arg
    putStr $ showResult result

loop :: IO ()
loop = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        putStr $ showResult $ evalLine "User input" input
        loop

showResult :: Either P.ParseError Double -> String
showResult (Right value) = printf "%f\n" value
showResult (Left err)    =
    "E" ++ showGraphicErrorPos (errorPos err) ++ "\n"
    ++ showLst "Expected" (expectedTokens err)

evalLine :: P.SourceName -> String -> Either P.ParseError Double
evalLine source s = eval <$> P.parse line source s

errorPos :: P.ParseError -> Int
errorPos = PP.sourceColumn . P.errorPos

showGraphicErrorPos :: Int -> String
showGraphicErrorPos 0 = ""
showGraphicErrorPos 1 = "^"
showGraphicErrorPos x = '-' : showGraphicErrorPos (x - 1)

showLst :: String -> [String] -> String
showLst _ []         = ""
showLst header items =
    header
    ++ " "
    ++ intercalate ", " (filter (not . null) items) ++ "\n"
