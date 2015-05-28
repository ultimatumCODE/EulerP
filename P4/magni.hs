magn :: (Integer, Integer) -> Integer
magn (x,y)
	| x `div` y == 0 = y
	| True = magn (x, y * 10)
	

isPalindrome :: (Integer, Integer, Integer) -> Integer
isPalindrome (q,order,order1)
	| order <= order1 = 1
	| (q `mod` order) `div` (order `div` 10) == (q `mod` order1) `div` (order1 `div` 10) = isPalindrome (q,order `div` 10,order1*10)
	| True = 0
	
	
findMaxPalindrome :: (Integer, Integer, Integer) -> Integer
findMaxPalindrome (x,y,z)
	| (x*x < z) && (z > 0)  = z
	| (isPalindrome (x*y,magn (x*y,1),10)== 1) && (x == y) && (x*y > z) = z
	| (isPalindrome (x*y,magn (x*y,1),10)== 1) && (x*y > z) = findMaxPalindrome (x-1,x-1,x*y)
	| y > 100 = findMaxPalindrome (x,y-1,z)
	| (y <=100 && x >100) = findMaxPalindrome (x-1,x-1,z)	
	| True = z

main = print (findMaxPalindrome (999,999,0))