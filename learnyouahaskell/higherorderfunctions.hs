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
  