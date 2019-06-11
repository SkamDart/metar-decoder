module Lib
    ( metar
      , Report
    ) where

import Control.Applicative
import Text.ParserCombinators.ReadP


data WindInfo = WindInfo { dir :: Int, speed :: Int, gusts :: Maybe Int }
    deriving (Show)

data Report = Report { station :: Airport, time :: Time, wind :: WindInfo }
    deriving (Show)

type Airport = String
type Time = (Int, Int, Int)

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)

uppercase :: ReadP Char
uppercase = satisfy (\char -> 'A' <= char && char <= 'Z')

digit :: ReadP Char
digit =  satisfy (\char -> '0' <= char && char <= '9')

space :: ReadP Char
space = satisfy (\char -> char == ' ')

numbers :: Int -> ReadP Int
numbers digits = fmap read (count digits digit)

timestamp :: ReadP (Int, Int, Int)
timestamp = do
    day <- numbers 2
    hour <- numbers 2
    minute <- numbers 2
    string "Z "
    if day < 1 || day > 31 || hour > 23 || hour < 0 || minute < 0 || minute > 59 then
        pfail
    else
        return (day, hour, minute)

airport :: ReadP String
airport = do
    code <- many1 (satisfy (\char -> char >= 'A' && char <= 'Z'))
    satisfy (== ' ')
    return code

toMPS :: String -> Int -> Int
toMPS unit speed =
    case unit of
        "KT" -> div speed 2
        "MPS" -> speed

gustParser :: ReadP Int
gustParser = do
    satisfy (== 'G')
    numbers 2 <|> numbers 3

windInfo :: ReadP WindInfo
windInfo = do
    direction <- numbers 3
    speed <- numbers 2 <|> numbers 3
    gusts <- option Nothing (fmap Just gustParser)
    unit <- string "KT" <|> string "MPS"
    string " "
    return (WindInfo
        direction
        (toMPS unit speed)
        (fmap (toMPS unit) gusts))

metar :: ReadP Report
metar = do
    code <- airport
    time <- timestamp
    wind <- windInfo
    return (Report code time wind)
