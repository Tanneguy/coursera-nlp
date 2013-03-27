{-
   Coursera NLP programming assignment 1
   Viterbi Algorithm

   Tanneguy Dulong  07/03/2013

-}

{-
  Part 0
  Comptage des fréquences d'émission
  Usage: cat datafile.train | count_emis.exe > datafile.train.rare
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
        m = emCount x'
        m' = foldrWithKey replaceRare empty m
    in showMap m' 

replaceRare :: String -> Int -> Map String Int -> Map String Int
replaceRare k a newMap = 
    if a < 5 
    then let cat = (last . words) k
         in  M.insertWith (+) ("_RARE_ "++cat) a newMap
    else M.insert k a newMap

emCount :: [String] -> Map String Int
emCount = L.foldl' condInsert empty


condInsert :: Map String Int -> String -> Map String Int 
condInsert m x = -- x`seq` m `seq`
                 if   x == ""
                 then m
                 else M.insertWith (+) x 1 m

rev :: String -> String
rev s = concat . (intersperse " ") . reverse . words $ s

showKV :: String -> Int -> String -> String
showKV k a result = result ++ (show a)  ++ " WORDTAG " ++ (rev k) ++ "\n" 
showMap m = M.foldrWithKey showKV "" m 
