main = do
  contents <- readFile "day1input.txt"
  let start = 50
  let moves = fmap parseMove $ lines contents
  print moves

data DialPosition int
data Direction = L | R
  deriving Show

parseMove :: String -> (Direction, Integer)
parseMove ('L':x) = (L, read x :: Integer)
parseMove ('R':x) = (R, read x :: Integer)
parseMove _ = (L, 0)

