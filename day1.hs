main = do
  contents <- readFile "day1input.txt"
  let start = 50
  let moves = fmap parseMove $ lines contents
  print moves

parseMove :: String -> Integer
parseMove ('L':x) = (-1 * read x :: Integer)
parseMove ('R':x) = (read x :: Integer)
parseMove _ = 0

