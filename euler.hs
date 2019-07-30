
-- factorial
factorial 0 = 1
factorial n = n*factorial (n-1)

-- euler taylor
-- 1/0! + 1/1! + 1/2! +...+ 1/n!
euler' :: Double -> [Double]
euler' n= map(factorial)[0..n]

euler :: Double -> Double
euler n = sum $ map(1/) $ euler' n


