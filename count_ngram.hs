{-
   Coursera NLP programming assignment 1
   Viterbi Algorithm

   Tanneguy Dulong  07/03/2013

-}

{-

   Part 0
   Fonctions de comptage pour
    - les émissions     134 WORDTAG I-GENE consensus 
    - les unigrames     134 1-GRAM I-GENE
    - les bigrames      134 2-GRAM O O
    - les trigrammes    134 3-GRAM O I-GENE O
-}

-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Char as C

main = do
    x <- getContents
    putStr $ process x

process :: String -> String
process x = 
    let x' = lines x
    in let z           = (concat . tailz) x' 
           (_, m, _)   = allCount 3 z
       in showMap m 

allCount :: Int -> [String] -> CntReg
allCount n = L.foldl' memCount (n, empty, replicate n "*")


type CntReg = (Int, Map [String] Int, [String])

memCount ::  CntReg  -> String -> CntReg
memCount (n, m, w) x =
    n `seq` m `seq` w `seq` 
    let 
        y = drop 1 (w++[x]) 
        (x1, x2, x3) = ([x], tail y, y)
    in 
    if (length y < n)
    then (n, m, y)
    else let
            m1 = condInsert x1 m
            m2 = condInsert x2 m1
            m3 = condInsert x3 m2
         in (n, m3, y)      

condInsert :: [String] -> Map [String] Int -> Map [String] Int 
condInsert x m =  
    if ("" `elem` x) 
    then m
    else M.insertWith (+) x 1 m


-- premz : liste des 1ers mots de chaque ligne  ou "\n" si la ligne vaut "\n"
-- tailz : liste des listes de mots de chaque ligne (excepté le 1er)  ou "\n" si la ligne vaut "\n"
-- tailz "toto titi tata" == ["titi", "tata"]
-- tailz "\n" == [""]
wordz "" = ["", ""]
wordz s  = words s
premz = P.map (head . wordz) 
tailz = P.map (tail . wordz)


showL l = concat (intersperse " " l)   
showKV k a result = result
             ++ (show a) ++ " " 
             ++ show(length k) ++ "-GRAM " 
             ++ (showL k) ++ "\n"
showMap m = foldrWithKey showKV "" m 