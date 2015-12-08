doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doublePlus1 start end = [(x * 2) + 1 | x <- [start..end]]

doublePlus1Evens start end = [(x * 2) + 1 | x <- [start..end], x `mod` 2 == 0]