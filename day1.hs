main = do
  -- part1
  part2
  part2test

part1 = do
  contents <- readFile "day1input.txt"
  let moves = fmap parseMove $ lines contents
  let zeros = foldl applyAndCount (50, 0) moves
  print $ snd zeros

part2 = do
  contents <- readFile "day1input.txt"
  let moves = fmap parseMove $ lines contents
  let zeros = foldl applyAndCountPasses (50, 0) moves
  print $ snd zeros

part2test = do
  print $ (applyAndCountPasses (0, 0) (0)) == (0,0)
  print $ (applyAndCountPasses (1, 0) (-2)) == (99,1)
  print $ (applyAndCountPasses (99, 0) (2)) == (1,1)
  print $ (applyAndCountPasses (0, 0) (0)) == (0,0)
  -- miss 0
  print $ (applyAndCountPasses (50, 0) (49)) == (99,0)
  print $ (applyAndCountPasses (50, 0) (-49)) == (1,0)
  -- land on 0
  print $ (applyAndCountPasses (50, 0) (50)) == (0,1)
  print $ (applyAndCountPasses (50, 0) (-50)) == (0,1)
  -- passes and misses 0
  print $ (applyAndCountPasses (50, 0) (100)) == (50,1)
  print $ (applyAndCountPasses (50, 0) (-100)) == (50,1)
  print $ (applyAndCountPasses (50, 0) (200)) == (50,2)
  print $ (applyAndCountPasses (50, 0) (-200)) == (50,2)
  -- passes and lands on 0
  print $ (applyAndCountPasses (50, 0) (150)) == (0,2)
  print $ (applyAndCountPasses (50, 0) (-150)) == (0,2)
  print $ (applyAndCountPasses (50, 0) (250)) == (0,3)
  print $ (applyAndCountPasses (50, 0) (-250)) == (0,3)
  print $ (applyAndCountPasses (50, 0) (150)) == (0,2)
  print $ (applyAndCountPasses (50, 0) (-150)) == (0,2)
  print $ (applyAndCountPasses (50, 0) (250)) == (0,3)
  print $ (applyAndCountPasses (50, 0) (-250)) == (0,3)

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

applyAndCountPasses :: (Integer, Integer) -> Integer -> (Integer, Integer)
applyAndCountPasses (position, zeroCount) move =
  let
    shiftedPosition = position + move -- -100
    newPosition = mod shiftedPosition 100 -- -100
    passes = abs $ div (shiftedPosition - newPosition) 100
    count = zeroCount + passes + if newPosition == 0 && move < 0 then 1 else 0
  in
    (newPosition, count)

