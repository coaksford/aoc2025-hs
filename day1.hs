main = do
  contents <- readFile "day1input.txt"
  let moves = fmap parseMove $ lines contents
  let zeros = foldl applyAndCount (50, 0) moves
  print $ snd zeros

parseMove :: String -> Integer
parseMove ('L':x) = (-1 * read x :: Integer)
parseMove ('R':x) = (read x :: Integer)
parseMove _ = 0

applyAndCount :: (Integer, Integer) -> Integer -> (Integer, Integer)
applyAndCount (position, zeroCount) move =
  let
    newPosition = mod (position + move) 100
    count = if newPosition == 0 then zeroCount + 1 else zeroCount
  in
    (newPosition, count)

