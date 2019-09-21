-- test


minimoDivisor :: Integer -> Integer
minimoDivisor n = mind n 2

mind :: Integer -> Integer -> Integer
mind n d | d^2>n = n
         | mod n d == 0 = d 
         | otherwise = mind n (d+1)

esPrimo :: Integer -> Bool
esPrimo p = (minimoDivisor p) == p

--lista de primos
--filtra [2..]
lsPrimos :: [Integer] -> [Integer]
lsPrimos (x:xs) = filter esPrimo xs


