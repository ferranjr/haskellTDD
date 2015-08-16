module StringCalculator where

import Data.List.Split
-- String Calculator TDD Kata

delimeterMark = "//"


readInt:: String -> Int
readInt []  = 0
readInt num = read num :: Int


hasDelimeter :: String -> Bool
hasDelimeter text = ( delimeterMark == take(length delimeterMark) text )


delimeter :: String -> Char
delimeter text
    | hasDelimeter text = ( head . head . splitWhen(=='\n') . drop(length delimeterMark) ) text
    | otherwise         = ','


body :: String -> String
body text
    | hasDelimeter text = drop(2 + length delimeterMark ) text
    | otherwise         = text


numbers :: [Int] -> [Int]
numbers =
    map(\n -> case () of
              _ | n < 0         -> error "negative numbers not allow"
                | n > 1000      -> 0
                | otherwise     -> n
    )

add :: String -> Int
add [] = 0
add text = ( sum . numbers . map readInt . splitWhen(\x -> x == ( delimeter text ) || x == '\n' ) . body ) text
