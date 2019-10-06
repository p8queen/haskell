
-- existe k Int, tq a^2 + a*b*k = 0, es decir a(a+bk) = 0 y sabemos 
-- a y b distontos 0. Entonces a+bk = 0 => a/b = -k. b es multiplo de a
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados a b = mod a b == 0

-- prod i=1  a 2n de (i^2 + 2i)
produ :: Integer -> Integer 
produ 1 = (1^2+2)*( 2^2+2*2 )
produ n = ((2*n)^2+2*n)*( (2*(n-1))^2+2*(n-1) ) * produ (n-1)

-- esCapicua :: Integer -> Bool
esCapicua n | n <= 9 = True
            | n <= 99 = ultimoDigito n == primerDigito n
            |otherwise = ultimoDigito n == primerDigito n && esCapicua (strip n)

--primer digito 454 -> 4 
primerDigito :: Integer -> Integer
primerDigito n | n<=9 = n
               |otherwise = primerDigito (div n 10)


--ultimo digito 454 -> 4 
ultimoDigito :: Integer -> Integer
ultimoDigito n = mod n 10

-- 5345 -> 34
strip :: Integer -> Integer
strip n = rightStrip (leftStrip n ) 2

-- 7894 -> 789 ,recive n>99
leftStrip :: Integer -> Integer
leftStrip n = div n 10

-- 1234 -> 234 ,recive n>99
rightStrip :: Integer -> Integer -> Integer
rightStrip n k | 10^k>n = mod n (10^(k-1)) 
               | otherwise = rightStrip n (k+1)


-- esCapicua' :: Integer -> Bool
esCapicua' n | n <= 9 = True
             | otherwise = esCapi n 1 (cantDigitos n)

cantDigitos :: Integer -> Integer
cantDigitos n | n<=9 = 1
              | otherwise = 1+ cantDigitos (div n 10)

esCapi :: Integer -> Integer -> Integer -> Bool
esCapi n j i | j>=i = True
             | digito n j /= digito n i = False
             | otherwise = esCapi n (j+1) (i-1)

digito :: Integer -> Integer -> Integer
digito n k = mod ( div n (10^(k-1)) ) 10




