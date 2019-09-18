
cuad :: Float -> Float
cuad n = -n^2+30*n

mayor p 0 = (p, cuad p)
mayor p q = mayor (my (p,cuad p) (q,cuad q) ) (q-1)

my p q | snd p > snd q = fst p
       | otherwise = fst q

