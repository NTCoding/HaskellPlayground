greeting :: String -> String
greeting "bob" = "Hey Bobarooni!"
greeting who = "Hey " ++ who

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

bestScore :: (Ord a) => (a, a) -> (a, a) -> a
bestScore (s1, s2) (s3, s4) = maximum [s1, s2, s3, s4]

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' [x] = x
sum' (x:xs) = x + sum' xs  

movieRating :: (RealFloat a) => a -> String
movieRating rating
  | rating < 3   = "Terrible movie"
  | rating < 5   = "Paint some walls instead"
  | rating < 7   = "Watchable"
  | rating < 10  = "Bloody good!"
  | rating == 10 = "Amazebombs"
  | otherwise    = "You can't code"
