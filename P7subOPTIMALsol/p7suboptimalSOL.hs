findprime :: (Int, [Int]) -> Int
findprime ( a , b)
	| a == 10000 = head b
	| True = findprime ( a+1, filter (\x -> x `mod` head b /= 0) b )

main = print (findprime( 0 , [2..] ) )
	