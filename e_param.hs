{-
   Sortie des paramètres d'émission
   word, tag => x ;  x = c(word, tag) / c(tag) 

   J'ai en entrée 123 WORDTAG I-GENE toto
   Usage: cat datafile.train.rare | e_params.exe > datafile.train.rare.e

-}
-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Char as C
import Text.Printf as T

main = do
    x <- getContents
    putStr $ process x


process x = let x' =  (L.map words . L.filter (/= "") . lines) x
            in let tagF = L.foldl' tagFreqCounter empty x'
                   emF  = L.foldl' emFreqCounter empty x'
               in let (_, eParam) = M.foldrWithKey emRatio (tagF, empty) emF
                  in showMap eParam

type EmFreq = Map (String, String) Double 
emRatio ::  (String, String) -> Int -> (TagFreq, EmFreq) -> (TagFreq, EmFreq)
emRatio (w, cat) n (tagF, m) = 
    let y = M.lookup cat tagF
    in if   y == Nothing 
       then (tagF, m) 
       else let (Just y') = y 
            in let m' = M.insert (w, cat) (fromIntegral n / fromIntegral y') m
               in (tagF, m')


type TagFreq = Map String Int
tagFreqCounter :: TagFreq -> [String] -> TagFreq
tagFreqCounter tagFreq [n,_,cat,w] = M.insertWith (+) cat (read n ::Int) tagFreq
tagFreqCounter tagFreq _           = tagFreq

type EmCount = Map (String, String) Int 
emFreqCounter :: EmCount -> [String] -> EmCount
emFreqCounter emCnt [n,_, cat,w] = M.insert (w, cat) (read n::Int) emCnt
emFreqCounter emCnt _            = emCnt

showT :: (String, String) -> String
showT (a,b) =  a ++ " " ++ b

showKV k a result = result
             ++ (showT k) ++ " " 
             ++ (printf "%.6f" a) ++ "\n"
showMap m = foldrWithKey showKV "" m

