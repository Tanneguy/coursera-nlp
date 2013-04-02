{-
   Sortie des paramètres d'émission
   word, tag => x ;  x = c(word, tag) / c(tag) 

  J'ai en entrée nnnn <n>-GRAM <cat> <cat>? <cat>?
  Usage: cat train.ngrams | q_params.exe >train.qparam

-}
-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.Maybe as B
import Data.List as L
import Data.Char as C
import Text.Printf as T

main = do
    x <- getContents
    putStr $ process x


process x = 
  let x'         =  (L.map words . L.filter (/= "") . lines) x
      ngramCnt   = L.foldl' ngramCounter empty x'
      (_,qmlMap) = M.foldrWithKey qmlAdd (ngramCnt, empty) ngramCnt
  in showMap qmlMap

type QParam = Map (String, [String]) Float
qmlAdd :: [String] -> Int -> (NGramCount, QParam) -> (NGramCount, QParam)  
qmlAdd ngram n3 (cnts, q)  =
  if length ngram /= 3
  then (cnts, q)
  else let x3  = last ngram                                            -- 
           x2  = init ngram
           n2  = fromJust (M.lookup x2 cnts)
       in let freq = (fromIntegral n3/ fromIntegral n2) :: Float
              q'   = M.insert (x3, x2) freq q
          in (cnts, q')


type NGramCount = Map [String] Int
ngramCounter :: NGramCount -> [String] -> NGramCount
ngramCounter nCount (n:_:grams) = M.insert grams (read n :: Int) nCount


showT :: (String, [String]) -> String
showT (a,b) =   (concat . intersperse " ") b ++  " " ++ a

showKV k a result = result
             ++ (showT k) ++ " " 
             ++ (show a) ++ "\n"
showMap m = foldrWithKey showKV "" m

