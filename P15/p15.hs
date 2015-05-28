factorial n = product [1..n]
magicnumber n = sum[((product [1..n]) `div` ((product [1..k])*product [1..(n-k)]))^2| k <- [0..n]]
main  = print (magicnumber 20)