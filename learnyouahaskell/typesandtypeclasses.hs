removeBandC :: [Char] -> [Char]
removeBandC xs = [c | c <- xs, not (c `elem` ['B', 'C'])]