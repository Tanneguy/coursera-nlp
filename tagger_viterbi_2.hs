{-
  Coursera-nlp-1 1st part
  Baseline tagger : produit le tag le + frequent (sans contexte)

  Usage : tagger_baseline.exe   <name>.emission   <name>.dev  
-}

-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.Maybe as B
import Data.List as L
import Data.Char as C
import System.Environment as E
import System.IO as IO
import Text.Printf as T
import Text.Regex as R (splitRegex)


main = do
    args <- getArgs
    eParamFile <- readFile (args !! 0)
    qParamFile <- readFile (args !! 1)
    textFile   <- readFile (args !! 2)
    putStr $ process eParamFile qParamFile textFile

{-
  la map du fichier X.emission est [Word, Tag, Emission]
  je veux une Map Word Map Emission Tag 

  le fichier a analyser est de type [Word]
  pour chaque Word je choisis le Tag qui a la plus haute Emission
  et je ressors [Word, Tag]-}

process e_param q_param text  = 
  let e_list = (L.map words . L.filter (/= "") . lines) eParamFile  -- [[word, cat, freq]]
      q_list = (L.map words . L.filter (/= "") . lines) qParamFile  -- [cat, cat, cat, freq]
      s_list = splitOn "\n\n" textFile                              -- [sentences]
      w_list = map lines s_list                                     -- [[word, word, ...]]
      e_map = L.foldl' mapAdd empty e_list                          -- construit la Map de tagging 
      q_map = L.foldl' mapAdd empty q_list

  in                        -- attention itérer sur les phrases !


piSentence s = 
  let n = length s
      k = [1..n] 
      ctx   = (q_map, e_map, s) 
      f     = pik ctx 1 u v   



mapAdd :: [String] -> Map [String] Float -> Map [String] Float
mapAdd entry  =
  let k = init entry
      s = last entry 
      a = read s :: Float
  in M.insert k a m


type Mot = String
type Tag = String
type EFreq = Map [String] Float
type QML   = Map [String] Float



{-
    postion -> sentence -> mapWordCat -> listeTags
-}
setK :: Int -> EFreq -> [String] 
setK k m =   
  if k <= 0 
  then ["*"]
  else let ks = M.keys m -- 
       in  map last ks   -- 

qml :: QML -> Tag -> Tag -> Tag -> Float
qml qMap w u v = fromJust (lookup [w,u,v] qMap)

exk :: EFreq -> Mot -> Tag -> Float
exk eMap x v = let exkMaybe = lookup [x, v]
               in if exkMaybe == Nothing 
                  then fromJust (lookup ["_RARE_", v])
                  else fromJust exkMaybe


piqe :: Contexte -> Int -> Tag -> Tag -> Tag -> Float
piqe cont k u v w = 
  let (qMap, eMap, s) = cont
      x = s !! k
      pikwu = pik cont (k-1) w u
      qmlWuv = qml qMap w u v
      exkv   = exk eMap x v
  in  pikwu * qmlWuv * exkv 


type Contexte= (QML, EFreq, [String])
pik :: Contexte -> Int -> Tag -> Tag -> Float
pik _ 0 "*" "*" = 1.0
pik cont k u v =
  let (q, e, s) = cont
      setW      = map (piqe cont k u v) (setK (k-2) e) -- [Float]
  in  max setW


maxTuple :: [(Float, Tag)] -> (Float, Tag)
maxTuple [] = (0,"")
maxTuple l = head . quickSort 

qSort [] = []
quickSort [(f, t):fts] = 
  let high = [(f1, t1) | (f1,t1) <-fts, f1 > f]
      mid  = [(f2, t2) | (f2,t2) <-fts, f2 == f]
      low  = [(f3, t3) | (f3,t3) <-fts, f3 < f]
  in qSort great ++ mid ++ qSort small 

