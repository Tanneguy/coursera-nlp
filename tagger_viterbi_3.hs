{-
  Coursera-nlp-1 1st part
  HMM tagger : produit le tag le + probable en fonction 
  des trigrammes de tags et des frequences d'émission

  Usage : tagger_viterbi.exe   rare.e  rare.q gene.test  
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
import Debug.Trace as D

main = do
    args <- getArgs
    eParamText <- readFile (args !! 0)
    qParamText <- readFile (args !! 1)
    text       <- readFile (args !! 2)
    putStr $ process eParamText qParamText text


process :: String -> String -> String -> String
process q_param e_param text  = 
  let e_map = makeMap e_param
      q_map = makeMap q_param
      w_list = phr [] (lines text)                             -- [[word, word], [word, word]]
      rx     = nub . L.map head $ keys e_map                   -- liste des mots dédoublonnée de e_map
  in let ctx = (q_map, e_map, rx)
--         res = [tagHmm ctx  (head w_list)]                            -- [[word tag\nnword tag\n ...][word tag]]
         res = L.map (tagHmm ctx) w_list                            -- [[word tag\nnword tag\n ...][word tag]]
      in unlines res


type Mot = String
type Tag = String
type EFreq = Map [String] Float
type QML   = Map [String] Float
type Ctx= (QML, EFreq, [String])




tagHmm :: Ctx -> [String] -> String
tagHmm ctx sentence =
--  let (pb, res, _) = trace "start viterbi 1 [] * * " viterbi ctx (1, [], ["*", "*"]) sentence
    let (pb, res, _) = viterbi ctx (1, [], ["*", "*"]) sentence
  in  unlines res                                                     -- "the D\ndog N\n .."

viterbi :: Ctx -> (Float, [String], [String]) -> [String] -> (Float, [String], [String])
viterbi _   (pb,res,uv) []    = (pb,res,uv)
viterbi ctx (pb,res,uv) (x:xs)  =   
  pb `seq` res `seq`
  let (q, e, _) = ctx
      ws     = ["I-GENE", "O"]                            -- ["O", "I-GENE"]  
--      pw     = L.foldl' (trace "maxFW" maxFW ctx rx x uv) (0,"") ws        -- (Float, String)  : meilleur score et tag associé 
      pw     = L.foldl' (maxFW ctx x uv) (0,"") ws        -- (Float, String)  : meilleur score et tag associé 
  in let (p, w) = pw
         res'   = res ++ [x ++ " " ++ w]
         uv'    = drop 1 (uv++[w])
         pb'    = pb * p
--     in  trace (" recursion viterbi \n\t"++show pb' ++" \n\t"++show res'++" \n\t"++show uv') viterbi ctx (pb', res', uv') xs
     in viterbi ctx (pb', res', uv') xs


maxFW :: Ctx -> Mot -> [Tag] -> (Float, Tag) -> Tag ->  (Float, Tag)
maxFW ctx x uv (f, w) w' =
  let (q,e,rx)= ctx
      quvw  = qml q (uv++[w'])
      exkv = if elem x rx
             then exk e [x,w']
             else exk e ["_RARE_", w']
      -- trace
{-      luvw  = uv++[w']
      logs  = "qml :" ++ show luvw ++" = "++ printf "%.6f" quvw ++ "\n"
              ++ "exk :"  ++ show [x,w'] ++ " = " ++ printf "%.6f" exkv ++ "\n"
              ++ " f' = " ++ show (quvw * exkv) ++ " <?> " ++ " f = " ++ show f ++ "\n"
   in let f'    = trace logs (quvw * exkv) 
-}

  in let f'    =  quvw * exkv
     in if f' > f
        then  (f', w')
        else  (f , w )
--        then trace ("freq, tag = "++show (f',w')++"\n\n" ) (f', w')
--        else trace ("freq, tag = "++show (f,w)++"\n\n" ) (f , w )



makeMap :: String -> Map [String] Float
makeMap param =
  let list = (L.map words . L.filter (/= "") . lines) param  -- [[word, cat, freq]]
      pmap = L.foldl' mapAdd empty list
  in pmap


mapAdd :: Map [String] Float -> [String] ->  Map [String] Float
mapAdd m entry  =
  let k = init entry
      s = last entry 
      a = read s :: Float
  in M.insert k a m


qml :: QML -> [Tag] -> Float
qml q uvw = let r = M.lookup uvw q
            in if r == Nothing 
               then 0
               else fromJust r

exk :: EFreq -> [String] -> Float
exk e xv = let exkMaybe = M.lookup xv e
           in if exkMaybe == Nothing 
              then 0
              else fromJust exkMaybe

phr :: [[String]] -> [String] -> [[String]]                         
phr ps []   = ps 
phr ps l_list = 
    let (newp, rest) = span (/="") l_list
    in phr (ps++[newp]) (tail rest) 

showKV :: [String] -> Float -> String -> String
showKV k a result = result ++ (show k) ++ (show a) ++ "\n" 
showMap m = M.foldrWithKey showKV "" m  
 