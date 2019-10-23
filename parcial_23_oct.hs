sumaFibonacci ::Integer->Integer 
sumaFibonacci 0 = 1
sumaFibonacci j = fibo j + sumaFibonacci (j-1)

fibo :: Integer -> Integer
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)



esDivisor :: Integer -> Integer -> Integer
esDivisor n k | mod n k == 0 = k
              | otherwise  = 0    

sumaDiv' :: Integer -> Integer -> Integer
sumaDiv' n 1 = 1
sumaDiv' n k = esDivisor n k + sumaDiv' n (k-1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDiv' n (n-1)  
                

esDefectivo :: Integer -> Bool
esDefectivo n = sumaDivisores n < n


comprimir :: [Integer] -> [(Integer,Integer)]
comprimir ns = comp' ns 1

--[1,1,1,2,3,3,4]
--comprimir (n:m:ns) = [(n,1), (m,1)] ++ comprimir ns

--r repetido, r=1 valor inicial 
comp' :: [Integer] -> Integer -> [(Integer,Integer)]
comp' [n] _ = [(n,1)]
comp' (n:m:ns) r | n/=m = [(n,r)] ++ comp' (m:ns) 1
                 | otherwise = comp' (m:ns) (r+1)


maximaDistancia :: [Integer] -> Integer
maximaDistancia xs = maxDist xs 0

-- maximaDistancia [1,6,2,7,8]  --> 5
-- d=0 valor inicial
maxDist :: [Integer] -> Integer -> Integer
maxDist [x] d = d
maxDist (x:y:xs) d | delta x y > d = maxDist (y:xs) (delta x y)
                   | otherwise = maxDist (y:xs) d


delta :: Integer -> Integer -> Integer
delta x y | x>y = x - y
          | otherwise = y - x






