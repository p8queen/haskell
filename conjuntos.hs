type Set a = [a]

vacio :: Set Integer
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
agregar x xs | elem x xs = xs 
             | otherwise = x:xs

incluido :: Set Integer -> Set Integer -> Bool
incluido [] zs = True
incluido (x:xs) zs | elem x zs = incluido xs zs
                   | otherwise = False

iguales :: Set Integer -> Set Integer -> Bool
iguales xs zs = cardinal xs == cardinal zs && incluido xs zs 

cardinal :: Set Integer -> Integer
cardinal [] = 0
cardinal (x:xs) = 1 + cardinal xs

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
agregarC xs zs | incluidoSet xs zs = zs
               |otherwise = xs:zs

incluidoSet :: Set Integer -> Set (Set Integer) -> Bool
incluidoSet xs [] = False
incluidoSet xs (z:zs) | iguales xs z = True
                      |otherwise = incluidoSet xs zs 
