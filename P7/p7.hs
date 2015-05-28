findprime :: (Int, [Int]) -> Int
findprime (a , b)
	| head b <= truncate (sqrt ((10001)* (log (10001)+  log(log 10001)))) = findprime ( a+1, filter (\x -> x `mod` head b /= 0) b )
	| True  = b!!(10001-1-a)

main = print (findprime( 0 , [2..] ))