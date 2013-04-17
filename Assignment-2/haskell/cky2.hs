{-      Coursera NLP programming assignment 2
        CKY Algorithm

        Tanneguy Dulong  05/04/2013

-}


import Data.Map as M
import Data.Maybe as B
import Data.Char as C
import Data.List as L

import Debug.Trace as D

import System.IO as IO
import System.Environment as E

import Text.Printf as T

main = do
    args <- getArgs
    nontermText <- readFile (args !! 0)
    unaryText   <- readFile (args !! 1)
    binaryText  <- readFile (args !! 2)
    text        <- readFile (args !! 3)
    putStr $ process nontermText unaryText binaryText text



process :: String -> String -> String -> String -> String
process nontermText unaryText binaryText text = 
    let nt_map = makeMap nontermText                    -- [X] -> Float

        un_map = makeMap unaryText                      -- [X, w] -> Float
        qu_map = M.mapWithKey (divParX nt_map) un_map   -- comptages divisés par nt_map (X)
        ur_map = M.foldrWithKey changeMap empty qu_map  -- [w] -> ([X], Float)

        bi_map = makeMap binaryText                     -- [X, Y, Z] -> Float
        qb_map = M.mapWithKey (divParX nt_map) bi_map   -- comptages divisés par nt_map (X) 
        br_map = M.foldrWithKey changeMap empty qb_map  -- [Y, Z] -> ([X], Float)

        nterm  = nub . L.map head $  keys nt_map

        w_list = L.map words $ lines text

    in let ctx = (ur_map, br_map, nterm)
           res = L.map (tagCKY ctx) w_list
--           res = [tagCKY ctx $ head w_list]
       in unlines res


type Ctx =  ( Map [String] (Map String Float)  -- qu : unary rules
            , Map [String] (Map String Float)  -- qb : binary rules
            , [String]                      -- nonterms list
            )


               
type Res =     (Int,   Map (Int, Int) (Map String (Float, Tree))   )

data Tree = Leaf String | Node String [Tree] 
    deriving (Eq, Show)  


tagCKY :: Ctx -> [String] -> String
tagCKY ctx sentence = 
    let res  = (1, empty)
        res' = L.foldl' (parseOne ctx) res sentence 
    in let  (n, in_res) = res' 
            win = if M.lookup (1, length sentence) in_res == Nothing
                  then empty
                  else in_res ! (1, length sentence)
            wwn = if M.lookup "SBARQ" win == Nothing
                  then (0,Node "" [])
                  else win ! "SBARQ"
            in let (score, tree) = wwn
               in toText tree


parseOne :: Ctx -> Res -> String -> Res
parseOne ctx res word  =
    let (qu, qb, nt) = ctx 
        (n, _)       = res
        res1 = add_un_pi ctx res (n,n) word
--        res1 = traceShow ("un_pi",n, word ) add_un_pi ctx res (n,n) word
    in  if n==1
        then nPlusUn res1 
        else let res2 = cky_pi ctx res1 (1, n)
--      else let res2 = traceShow ("cky", n ) cky_pi ctx res1 (1, n)
             in nPlusUn res2

nPlusUn :: Res -> Res
nPlusUn (n, x) = (n+1, x)

cky_pi :: Ctx -> Res -> (Int, Int) -> Res
cky_pi ctx res (1, n) = 
    let scopes = combos 1 n
    in  L.foldl' (add_pi ctx) res scopes
--  in  L.foldl' (traceShow ("add_pi", scopes) add_pi ctx) res scopes

add_pi :: Ctx -> Res -> ((Int, Int),(Int, Int)) -> Res  
add_pi ctx res (ij,kl) =
    let (n, in_res)  = res
        r1 = M.lookup ij in_res   -- Maybe Map [String] -> Float
        r2 = M.lookup kl in_res    -- Maybe
    in if r1 == Nothing || r2 == Nothing
       then res
       else let r1_tags = keys $ fromJust r1
                r2_tags = keys $ fromJust r2
                allYZ = [ [y,z] | y <- r1_tags, z <- r2_tags ]
--          in let res' = L.foldl' (traceShow ("bi_pi", ij, kl, r1_tags, r2_tags) add_bi_pi ctx (ij,kl)) res allYZ
            in let res' = L.foldl' (add_bi_pi ctx (ij,kl)) res allYZ
               in res'


combos i l = 
    if i == l 
    then []
    else let js = [i .. (l-1)]
             ints = [ ((i,j),(j+1,l)) |  j <- js ]
         in combos (i+1) l ++  ints


add_un_pi :: Ctx -> Res -> (Int,Int) -> String -> Res
add_un_pi ctx res ii word =
    let (qu, qb, nt) = ctx
        (n, in_res)  = res
        xs = if M.lookup [word] qu  == Nothing
             then qu ! ["_RARE_"]                 -- Map String Float
             else qu ! [word]                     -- Map String Float
        r  = if M.lookup ii in_res == Nothing
             then empty 
             else in_res ! ii                      -- Map String (Float, Tree)
    in  let tag_map = M.foldrWithKey (buildNode word) empty xs 
        in  (n, M.insert ii tag_map in_res )




buildNode :: String -> String -> Float -> Map String (Float, Tree) -> Map String (Float, Tree)
buildNode word tag score m = 
    let tree = Node tag [Leaf word]
    in  M.insert tag (score, tree) m               -- insert ignore les doublons


add_bi_pi :: Ctx -> ((Int,Int),(Int,Int)) -> Res -> [String] -> Res
add_bi_pi ctx ((i,j),(k,l)) res cnf_right =
    let (qu, qb, nt) = ctx
        x  = M.lookup cnf_right qb     -- Maybe (Map String Float)
    in if x == Nothing
       then res
       else let (n, in_res)      = res
                r1 = in_res ! (i,j)                                -- ij
                r2 = in_res ! (k,l)                                -- ij
                (score_ij, tree_ij) = r1 ! (head $ take 1 cnf_right)   -- Y  pas de risque d'absence   
                (score_kl, tree_kl) = r2 ! (head $ drop 1 cnf_right)   -- Z  cnf_right est construite via keys
                scoreil = score_ij * score_kl
                resil = if M.lookup (i,l) in_res  == Nothing 
                        then empty 
                        else in_res ! (i,l)
            in  let tag_map = M.foldrWithKey (build_bi_Node tree_ij tree_kl scoreil) empty $ fromJust x 
                in  (n, M.insert (i,l) tag_map in_res )


build_bi_Node :: Tree -> Tree -> Float -> String -> Float -> Map String (Float, Tree) -> Map String (Float, Tree)
build_bi_Node treeij treekl scoreil tag score  m = 
    let newtree  = Node tag [treeij, treekl]
        newscore = score*scoreil
    in if member tag m 
       then let (s,_) = m ! tag 
            in if s > newscore
               then m                                                -- on garde la solution précédente
               else M.insert tag (newscore, newtree) m               -- on remplace l'ancien résultat
       else M.insert tag (newscore, newtree) m



{-  Fonctions utilitaires  
-}

divParX :: Map [String] Float -> [String] -> Float -> Float
divParX nt_map key count = 
        let k = take 1 key                      --  on veut une clef de type ["NP"]
            c = nt_map ! k 
        in  count / c
        

mapAdd :: Map [String] Float -> [String] ->  Map [String] Float
mapAdd m entry  =
  let k = drop 2 entry          -- count, type éliminés, on conserve [X,Y,Z] (ou [X,w])
      s = take 1 entry 
      a = read (head s) :: Float       
  in M.insert k a m             

changeMap :: [String] -> Float -> Map [String] (Map String Float) -> Map [String] (Map String Float) 
changeMap k a m =
  let newk = drop 1 k                                   -- [X,Y,Z]-> Float  ==> [Y,Z]-> Map X Float
      tagt = head k                                     -- [X,w]-> Float    ==> [w]-> Map X Float
      newv = singleton tagt a  
  in if M.lookup newk m == Nothing
     then M.insert newk newv m                          -- première possibilité pour w
     else let newv' = M.insert tagt a $ m!newk          -- ajout de la nouvelle possibilité pour w
          in M.insert newk newv' m                      -- remplacement de l'ancienne map par la map augmentée


makeMap :: String -> Map [String] Float
makeMap param =
  let list = (L.map words . L.filter (/= "") . lines) param  -- [[count, type, key1, key2, key3]]
      pmap = L.foldl' mapAdd empty list
  in pmap


{-
        Sortie sous forme de texte       
-}
foldToText :: [Tree] -> String
foldToText []  = ""
foldToText [t] = toText t
foldToText (t:ts) =  toText t++", "++foldToText ts

toText :: Tree -> String
toText (Node s [Leaf a]) = "[" ++ show s++  ", " ++ show a ++ "]"  
toText (Node s ts  )        = "[" ++ show s ++ ", "++ foldToText ts ++"]"

outerFold :: [Tree] -> [String]
outerFold ts = L.map toText ts


prettyShow :: Res -> String
prettyShow x = show x
