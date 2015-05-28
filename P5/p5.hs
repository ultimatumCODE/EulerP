findnumber :: ([Integer] , [Integer], [Integer]) -> Integer
findnumber ( a , b , c )
	| a == [ ] = findnumber ( c ++ [head b]  , tail b , c ++ [head b])
	| b == [ ] = product c
	| head b  `mod` last a  /= 0 = findnumber ( init a , b , c)
	| head b  `mod` last a  == 0 = findnumber ( init a , [head b `div` last a ] ++ tail b  , c)
	
p =   findnumber ( [2] , [3..20] , [2] ) 
main = print p