-- cabal install --lib split --package-env .
import Data.List.Split

main = do
  part1

tests = do
  print $ 0 == findPattern "10"
  print $ 0 == findPattern "101"
  print $ 0 == findPattern "111"
  print $ 1010 == findPattern "1010"
  print $ 0 == findPattern "101010"

part1 = do
  contents <- readFile "day2input.txt"
  let pairs = fmap (splitOn "-") $ splitOn "," contents
  let intEndpoints = fmap parseInts pairs
  let candidates = concat $ fmap getRange intEndpoints
  let sum = sumMatchesInRange candidates
  print sum

parseInts :: [String] -> [Integer]
parseInts [] = []
parseInts (x:xs) = (parseInt x):(parseInts xs)

parseInt :: String -> Integer
parseInt x = read x :: Integer

findPattern :: String -> Integer
findPattern x = let
    isEvenLength = (mod (length x) 2) == 0
    halfLength = div (length x) 2
    firstHalf = take halfLength x
    lastHalf = drop halfLength x
  in
    if firstHalf /= lastHalf
    then 0
    else parseInt x

getRange :: [Integer] -> [Integer]
getRange (start:end:_) = [ start .. end ]

sumMatchesInRange :: [Integer] -> Integer
sumMatchesInRange [] = 0
sumMatchesInRange (x:xs) = (findPattern (show x)) + sumMatchesInRange xs
