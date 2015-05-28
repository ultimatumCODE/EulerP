solve :: (Integer, Integer) -> (Integer, Integer)
solve (x, y) 
	| y >= x = (x, y)
	| x `mod` y == 0 = solve (x `div` y, y)
	| True = solve (x, y + 1)

main = print (fst (solve (600851475143, 2)))