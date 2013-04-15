-- sachant l :: [Int] et l' :: [Int]

-- pour chaque String, choisir dans l / l' la valeur la + grande 
-- tracer la somme, les valeurs et les choix l l'



-- π(i, j, x) 

import  System.IO as IO
import System.Environment as E 

main = do 
	text <- getContents 
	putStr text



l1 = [ (x*x*x - 10*x*x + 50) | x <- [1..10]]
l2 = [ (20*x*x - 50*x) | x <- [1..10]]


-- je démarre à la position 0 avec pas de bp et une somme nulle

π (0) = (0,[])

π (n) = 
	let (s , bps) =  π (n-1) 
		(s', bp') = if l1 !! n > l2 !! n 
					then (l1 !! n, "l1")
					else (l2 !! n, "l2")
	in 	(s+s', bp':bps)

