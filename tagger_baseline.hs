{-
  Coursera-nlp-1 1st part
  Baseline tagger : produit le tag le + frequent (sans contexte)

  Usage : tagger_baseline.exe   <name>.emission   <name>.dev  
-}




-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Char as C
import System.Environment as E
import System.IO as IO
import Text.Printf as T


main = do
    args <- getArgs
    emitText <- readFile (args !! 0)
    dataText <- readFile (args !! 1)
    putStr $ process emitText dataText 


{-
  la map du fichier X.emission est [Word, Tag, Emission]
  je veux une Map Word Map Emission Tag 

  le fichier a analyser est de type [Word]
  pour chaque Word je choisis le Tag qui a la plus haute Emission
  et je ressors [Word, Tag]

-}

process emitText dataText = 
  let emitList = (L.map words . L.filter (/= "") . lines) emitText  -- texte sous forme de liste
      dataList = lines dataText                                     -- texte sous forme de liste non filtrée 
      emitMap = L.foldl' highEmitCounter empty emitList             -- construit la Map de tagging max
  in let lResult = L.map (tagLine emitMap) dataList                -- applique le tagging
--  in unlines lResult
--  in showMap emitMap

type EmitTag = Map String (Float, String)
highEmitCounter :: EmitTag -> [String] -> EmitTag
highEmitCounter emitMap [word, tag, float] = 
  let floatNum = read float :: Float
      tagMap = (floatNum, tag)
      x = M.lookup word emitMap         -- lookup retourne Maybe (Float, Tag)
  in if x == Nothing
     then M.insert word tagMap emitMap
     else let (Just x') = x               -- type x' = (Float, Tag)
          in  if   fst x' > floatNum 
              then emitMap
              else M.update (\x -> Just tagMap) word emitMap


  
tagLine :: EmitTag -> String -> String
tagLine _ "" = ""
tagLine mEmit line  = 
  let x = M.lookup line mEmit 
      y = if x == Nothing 
          then y =  M.lookup "_RARE_" mEmit 
      -- then error "line not found ! -->> \n["++line++"]\n<<--"
      else let (Just x') = x
           in  line++" "++(snd x') 
 

showKV :: String -> (Float,String) -> String -> String
showKV k (f, t) result = result ++ k ++ ": \t"++t++"\t"++show(f)++ "\n" 
showMap m = M.foldrWithKey showKV "" m  
