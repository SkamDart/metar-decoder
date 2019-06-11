module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.ParserCombinators.ReadP

import Lib

process :: String -> IO ()
process line = putStrLn $ show $ fst $ head (readP_to_S metar line)

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine "Metar> "
            case minput of
                Nothing -> outputStrLn "goodbye!"
                Just input -> (liftIO $ process input) >> loop
