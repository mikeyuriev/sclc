module Main where

import Control.Monad.State (unless, when)
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
    if null args
        then loop
        else do
            let exprStr = head args
            let result = evalLine "Command line" exprStr
            when (isLeft result) $ do putStrLn $ " " ++ exprStr
            putResult result

loop :: IO ()
loop = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        putResult $ evalLine "User input" input
        loop

putResult :: Either P.ParseError Double -> IO ()
putResult (Right value) = putStrLn $ printf "%f" value
putResult (Left err)    = do
    putStrLn $ "E" ++ showGraphicErrorPos (errorPos err)
    putList "Expected" $ expectedTokens err

evalLine :: P.SourceName -> String -> Either P.ParseError Double
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
        putStr $ header ++ " "
        putStrLn $ intercalate ", " $ filter (not . null) items
