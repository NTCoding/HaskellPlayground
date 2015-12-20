loves :: String -> String -> String
loves a b = a ++ " loves " ++ b

-- example of currying this with infix operations
nickLoves = ("Nick" `loves`)
lovesProgramming = (`loves` "programming")

repeatN :: (a -> a) -> a -> Int -> a
repeatN _ a 0 = a
repeatN f a n = repeatN f (f a) (n - 1)

zipFilter :: ((a,b) -> Bool) -> [a] -> [b] -> [(a,b)]
zipFilter _ [] _ = []
zipFilter _ _ [] = []
zipFilter f (a:as) (b:bs) = 
  case f (a,b) of True  -> (a,b) : zipFilter f as bs
                  False -> zipFilter f as bs


funcDestruct :: (String -> String -> c) -> (String -> String -> String)
funcDestruct f x y = "We got " ++ x ++ y

funco :: String -> String -> String
funco = \x y -> "YOLO " ++ x ++ y

sumSquareOddsBelow10k =
  let odds = [1,3..9999]
      squared = [n * n | n <- odds]
  in  sum squared

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n =
  case n `mod` 2 of 0 -> n : collatz (n `div` 2)
                    1 -> n : collatz ((n * 3) + 1)

sumEvens :: [Int] -> Int
sumEvens xs = foldl (\acc x -> if (x `mod` 2 == 0) then acc + x else acc) 0 xs


  