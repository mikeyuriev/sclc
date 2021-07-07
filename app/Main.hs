module Main where

import Control.Monad.State (unless)
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import SmallCalc.Eval
import SmallCalc.Parser
import SmallCalc.Types

data ErrorShowing
    = Short
    | Long
    deriving Eq

main :: IO ()
main = do
    args <- getArgs
    let ctx = Context [] 0
    bool (putStr (handleArgs ctx args)) (loop ctx) (null args)

handleArgs :: Context -> [String] -> String
handleArgs _   []
    = ""
handleArgs ctx (s:ss)
    =  showResult Short ctx' ++ "\n"
    ++ (handleArgs $ fromRight ctx ctx') ss
    where
        ctx' = updateContext s ctx

loop :: Context -> IO ()
loop ctx = do
    putStr ">"
    hFlush stdout
    input <- getLine
    unless (null input) $ do
        let ctx' = updateContext input ctx
        putStrLn $ showResult Long ctx'
        loop $ fromRight ctx ctx'

updateContext :: String -> Context -> Either Error Context
updateContext str ctx =
    parseStatement str >>= execStatement ctx

showResult :: ErrorShowing -> Either Error Context -> String
showResult errShowing (Left err)
    = showError errShowing err
showResult _ (Right (Context _ result))
    = printf "%f" result

showError :: ErrorShowing -> Error -> String
showError Long (ESyntax pos expected)
    =  "!"
    ++ showGraphicErrorPos pos
    ++ showLst "Expected" expected
showError _ (ESyntax pos _)
    =  "Syntax error at position " ++ show pos
showError _ EDivisionByZero
    =  "Division by zero"
showError _ (EUndefined name)
    =  "Undefined name " ++ name

showGraphicErrorPos :: Int -> String
showGraphicErrorPos 0 = ""
showGraphicErrorPos 1 = "^"
showGraphicErrorPos x = '-' : showGraphicErrorPos (x - 1)

showLst :: String -> [String] -> String
showLst _ []         =  ""
showLst header items = "\n" ++ header ++ " " ++ intercalate ", " items
