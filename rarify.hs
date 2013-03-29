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
    let y = lines x                                        -- ["toto O","","titi I-GENE",...]
        m = emCount y                                      -- fromList[("toto O",55),("",55000),("titi I-GENE",4)]
        m' = M.filter (<5) m                                -- fromList[("titi I-GENE",4)]
        r = replaceRare m'
        t = L.map r y 
    in unlines t                                          -- 
--    in showMap m'

replaceRare :: Map String Int -> String -> String
replaceRare m textLine = 
    let listLine = wordz textLine                          -- ["toto","O"]
        txtWord  = head listLine
        txtCat   = last listLine
    in if (member textLine m) 
       then "_RARE_ " ++ txtCat
       else textLine


emCount :: [String] -> Map String Int
emCount = L.foldl' condInsert empty

condInsert :: Map String Int -> String -> Map String Int 
condInsert m x = if   x == ""
                 then m
                 else M.insertWith (+) x 1 m


wordz "" = ["", "STOP"]
wordz s  = words s


showKV k a result = result
             ++ k ++ " " 
             ++ (show a) ++ "\n"
showMap m = foldrWithKey showKV "" m