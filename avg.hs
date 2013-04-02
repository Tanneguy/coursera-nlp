sum :: Num a => [a] -> a
sum xs

sumCnt :: Num a => (Int, a) -> a (Int, a)
sumCnt (n, x) y = (n+1, x+y) 

moyenne xs = 
	n `seq` 
	let (n, s) = foldl' (0, 0) xs
	in s/n




