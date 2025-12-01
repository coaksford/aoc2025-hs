main = do
  contents <- readFile "day1input.txt"
  let start = 50
  let moves = lines contents
  print moves

