

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b) 


mayorComunDivisor :: Integer -> Integer -> Integer
mayorComunDivisor a b = mdcd a b (min a b)

mdcd :: Integer -> Integer -> Integer -> Integer
mdcd a b k | mod a k == 0 && mod b k == 0 = k
           | otherwise = mdcd a b (k-1)


minimoDivisor :: Integer -> Integer
minimoDivisor n = mind n 2

mind :: Integer -> Integer -> Integer
mind n d | d^2>n = n
         | mod n d == 0 = d 
         | otherwise = mind n (d+1)

minDivMax :: Integer -> Integer -> Integer
minDivMax _ 1 = 1
minDivMax 1 _ = 1
minDivMax a b | mda == mdb = mda * minDivMax (div a mda) (div b mda)
              |mda < mdb = minDivMax (div a mda) b
              |otherwise = minDivMax a (div b mdb) 
              where 
                mda = minimoDivisor a
                mdb = minimoDivisor b

emcd :: Integer -> Integer -> (Integer, Integer, Integer) 
emcd a 0 = (a,1,0)
emcd a b = (g,s,t)
           where (g,s',t') = emcd b (mod a b )
                 s = t'
                 t = s'-t'*q
                 q = div a b

tieneSolucion :: Integer -> Integer -> Integer -> Bool
tieneSolucion a b m = mod b ( mcd a m ) == 0

solucionParticular :: Integer -> Integer -> Integer -> Integer
solucionParticular a b m | tieneSolucion a b m = s * (div b g)
                         where (g,s,t) = emcd a m


solucionGeneral :: Integer -> Integer -> Integer -> (Integer,Integer)
solucionGeneral a b m | tieneSolucion a b m = (s * ( div b g ), div m g)
                      where (g,s,t) = emcd a m   





