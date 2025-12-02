-- cabal install --lib split --package-env .
import Data.List.Split

main = do
  part1
  part2

tests = do
  print $ 0 == findPattern "10"
  print $ 0 == findPattern "101"
  print $ 0 == findPattern "111"
  print $ 1010 == findPattern "1010"
  print $ 0 == findPattern "101010"
  print $ False == searchCandidatesFrom "223" "222223"
  print $ 565656 == (findMultiPattern "565656")

part1 = do
  contents <- readFile "day2input.txt"
  let pairs = fmap (splitOn "-") $ splitOn "," contents
  let intEndpoints = fmap parseInts pairs
  let candidates = concat $ fmap getRange intEndpoints
  let sum = sumMatchesInRange candidates
  print sum

part2 = do
  contents <- readFile "day2input.txt"
  let pairs = fmap (splitOn "-") $ splitOn "," contents
  let intEndpoints = fmap parseInts pairs
  let candidates = concat $ fmap getRange intEndpoints
  let sum = sumMultiMatchesInRange candidates
  print $ sum

parseInts :: [String] -> [Integer]
parseInts [] = []
parseInts (x:xs) = (parseInt x):(parseInts xs)

parseInt :: String -> Integer
parseInt x = read x :: Integer

findPattern :: String -> Integer
findPattern "" = 0
findPattern x = let
    isEvenLength = (mod (length x) 2) == 0
    halfLength = div (length x) 2
    firstHalf = take halfLength x
    lastHalf = drop halfLength x
  in
    if firstHalf /= lastHalf
    then 0
    else parseInt x

findMultiPattern :: String -> Integer
findMultiPattern input = let
    tooShort = (length input) < 2
    longestRepeatingMatchCandidate = take (div (length input) 2) input
  in
    if tooShort
    then 0
    else if searchCandidatesFrom longestRepeatingMatchCandidate input 
    then parseInt input
    else 0

searchCandidatesFrom :: String -> String -> Bool
searchCandidatesFrom "" _ = False
searchCandidatesFrom needle haystack = let
    fittable = mod (length haystack) (length needle) == 0
    shortenedNeedle = take ((length needle) - 1) needle
    found = haystackIsAllNeedles needle haystack
  in
    if not fittable
    then searchCandidatesFrom shortenedNeedle haystack
    else found || searchCandidatesFrom shortenedNeedle haystack

haystackIsAllNeedles :: String -> String -> Bool
haystackIsAllNeedles needle haystack = let
    substring = drop (length needle) haystack
    isRepeating = (take (length needle) haystack) == needle
    exactMatch = needle == haystack
  in
    if exactMatch
    then True
    else if isRepeating
      then haystackIsAllNeedles needle substring
      else False

getRange :: [Integer] -> [Integer]
getRange (start:end:_) = [ start .. end ]

sumMatchesInRange :: [Integer] -> Integer
sumMatchesInRange [] = 0
sumMatchesInRange (x:xs) = (findPattern (show x)) + sumMatchesInRange xs

sumMultiMatchesInRange :: [Integer] -> Integer
sumMultiMatchesInRange [] = 0
sumMultiMatchesInRange (x:xs) = (findMultiPattern (show x)) + sumMultiMatchesInRange xs
