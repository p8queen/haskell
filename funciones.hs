-- largo lista
largo []=0
largo (x:xs) = 1 + largo xs

cuadrado::Integer -> Integer
cuadrado x = x * x

menor::(Integer, Integer) -> Integer
menor (x,y) = if x <= y then x else y

sumatoria::[Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- reverse de una lista
dadovuelta::[a] -> [a]
dadovuelta [] = []
dadovuelta (x:xs)= dadovuelta xs ++ [x]

-- busqueda del mayor (max) de una lista
-- diferentes implementaciones
-- ej: mayor [5,3,2,9,6]
mayor::[Integer] -> Integer
mayor [] = 0
mayor (x:xs) | x>=mayor(xs) = x
             | otherwise = mayor(xs) 

mayor' [x] = x
mayor' [x,y] | x>y = x 
             | otherwise = y
mayor' (x:y:xs) | x>y = mayor'(x:xs)
                | otherwise = mayor'(y:xs)

-- serie infinita de fibonacci
-- ej: take 20 fib 
fib' = 1:1:[a+b|(a,b)<-zip fib' (tail fib')]
fib@(1:xs) = 1:1:[a+b|(a,b)<-zip fib xs]

-- lista de binarios en forma de duplas (pos,valor)
-- ej: binarios 20
binarios hasta = zip [0..] (map (2^) [0..hasta])

-- devuelve en una lista el numero binario
-- ej: pasaBinario 5 -> [1,0,1]
pasaBinario 0 = [0]
pasaBinario 1 = [1]
pasaBinario n = pasaBinario (div n 2) ++ [mod n 2]

-- busca en una lista ordenada 
-- aplicando busqueda binaria
busqBinaria n [x] = n == x
busqBinaria n lista | n < (lista !! p) = busqBinaria n (take p lista)
                    | otherwise = busqBinaria n (drop p lista)
                    where p = div (length lista) 2

-- primos criba erastano
-- lista infinita en dos lineas de codigo

primos' (x:xs) = x:(primos' . filter (\r -> mod r x /= 0) ) xs
primos = 2:primos' [3,5..]

-- ordena una lista
-- va pasando valores de una lista llena a una lista vacia 
-- en forma ordenada
-- ej listaSort [6,4,5,9,8,2]
insertarOrdenado valor [] = valor:[]
insertarOrdenado valor (x:xs) | valor <= x = valor:x:xs
                              | otherwise = x:insertarOrdenado valor xs

listaSort ::Ord a=> [a]->[a]
listaSort = foldr (insertarOrdenado) []
-- fin ordenar lista


-- mcd algoritmo euclides 
-- ej: mcd 28 16
mcd a 0 = a
mcd a b = mcd b (rem a b)


