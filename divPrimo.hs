-- test


minimoDivisor :: Integer -> Integer
minimoDivisor n = mind n 2

mind :: Integer -> Integer -> Integer
mind n d | mod n d == 0 = d
         | otherwise = mind n (d+1)

esPrimo :: Integer -> Bool
esPrimo p = (minimoDivisor p) == p

