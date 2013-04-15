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
        ur_map = M.foldrWithKey changeMap empty un_map  -- [w] -> ([X], Float)

        bi_map = makeMap binaryText                     -- [X, Y, Z] -> Float
        qb_map = M.mapWithKey (divParX nt_map) bi_map   -- comptages divisés par nt_map (X) 
        br_map = M.foldrWithKey changeMap empty bi_map  -- [Y, Z] -> ([X,Y,Z], Float)

        nterm  = nub . L.map head $  keys nt_map

        w_list = phr [] (lines text)                 -- [[word, word], [word, word]]
  
    in let ctx = (ur_map, br_map, nterm)
           res = L.map (tagCKY ctx) w_list
       in unlines res


type Ctx =  ( Map [String] (String, Float)  -- qu : unary rules
            , Map [String] (String, Float)  -- qb : binary rules
            , [String]                      -- nonterms list
            )

-- Res =  Map (Int, Int) -> Map [String] -> (Float, Tree) 

type ResExt =  (Int,   Res) 
                
type Res =  (   Int
            ,   Map (Int, Int) (Map String (Float, Tree)) 
            )

data Tree = Leaf String | Node String [Tree] 
    deriving (Eq)  


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
    in let res2 = cky_pi ctx res1 (1, n)
       in nPlusUn res2

nPlusUn :: Res -> Res
nPlusUn (n, x) = (n+1, x)

cky_pi :: Ctx -> Res -> (Int, Int) -> Res
cky_pi ctx res (1, n) = 
    let scopes = [ ((1,i),(i+1,n)) | i <- [1..n-1]]                        
    in  L.foldl' (add_pi ctx) res scopes

add_pi :: Ctx -> Res -> ((Int, Int),(Int, Int)) -> Res  
add_pi ctx res (ij,kl) =
    let (qu, qb, nt) = ctx
        (n, in_res)  = res
        r1 = M.lookup ij in_res   -- Maybe Map [String] -> Float
        r2 = M.lookup kl in_res    -- Maybe
    in if r1 == Nothing || r2 == Nothing
       then res
       else let r1_tags = keys $ fromJust r1
                r2_tags = keys $ fromJust r2
                combos  = [ [y,z] | y <- r1_tags, z <- r2_tags ]
            in let res' = L.foldl' (add_bi_pi ctx (ij,kl)) res combos
            in res'


add_un_pi :: Ctx -> Res -> (Int,Int) -> String -> Res
add_un_pi ctx res ii word =
    let (qu, qb, nt) = ctx
        (n, in_res)  = res
        x  = if M.lookup [word] qu  == Nothing
             then qu ! ["_RARE_"]
             else qu ! [word]   
        r  = if M.lookup ii in_res == Nothing
             then empty 
             else in_res ! ii
    in  let (cnf_tag, cnf_score) = x
            tag_score = cnf_score 
            tag_tree  = Node cnf_tag [Leaf word]
            tag_map   = M.insert cnf_tag (tag_score, tag_tree) r 
        in  (n, M.insert ii tag_map in_res )


add_bi_pi :: Ctx -> ((Int,Int),(Int,Int)) -> Res -> [String] -> Res
add_bi_pi ctx ((i,j),(k,l)) res cnf_right =
    let (qu, qb, nt) = ctx
        x  = M.lookup cnf_right qb     -- Maybe ([String], Float) ou (String, Float) ?
    in if x == Nothing
       then res
       else let (n, in_res)      = res
                r1 = in_res ! (i,j)                                -- ij
                r2 = in_res ! (k,l)                                -- kl
                (score_ij, tree_ij) = r1 ! (head $ take 1 cnf_right)   -- Y  pas de risque d'absence   
                (score_kl, tree_kl) = r1 ! (head $ drop 1 cnf_right)   -- Z  cnf_right est construite via keys
                resil = if M.lookup (i,l) in_res  == Nothing 
                        then empty 
                        else in_res ! (i,l)
            in  let (cnf_tag, cnf_score) = fromJust x
                    tag_score            = cnf_score * score_ij * score_kl
                    tag_tree             = Node cnf_tag [tree_ij, tree_kl]
                    tag_map              = M.insert cnf_tag (tag_score, tag_tree) resil
                in (n, M.insert (i,l) tag_map in_res)


{-  Fonctions utilitaires  
-}

divParX :: Map [String] Float -> [String] -> Float -> Float
divParX nt_map key count = 
        let k = take 1 key                      --  on veut une clef de type ["NP"]
            c = nt_map ! k 
        in  count / c
        

mapAdd :: Map [String] Float -> [String] ->  Map [String] Float
mapAdd m entry  =
  let k = drop 2 entry          -- count, type éliminés, on conserve key1,key2,key3
      s = take 1 entry 
      a = read (head s) :: Float       
  in M.insert k a m             


changeMap :: [String] -> Float -> Map [String] (String, Float) -> Map [String] (String, Float) 
changeMap k a m =
  let newk = drop 1 k          -- count, type éliminés, on conserve key1,key2,key3
      tagt = take 1 k 
      newv = (head tagt, a)       
  in M.insert newk newv m             

makeMap :: String -> Map [String] Float
makeMap param =
  let list = (L.map words . L.filter (/= "") . lines) param  -- [[count, type, key1, key2, key3]]
      pmap = L.foldl' mapAdd empty list
  in pmap


phr :: [[String]] -> [String] -> [[String]]                         
phr ps []   = ps 
phr ps l_list = 
    let (newp, rest) = span (/="") l_list
    in phr (ps++[newp]) (tail rest) 

{-
        Sortie sous forme de texte       
-}
foldToText :: [Tree] -> String
foldToText [t] = toText t
foldToText (t:ts) =  toText t++","++foldToText ts

toText :: Tree -> String
toText (Node s [Leaf a]) = "[" ++ show s++  "," ++ show a ++ "]"  
toText (Node s ts  )        = "[" ++ show s ++ ","++ foldToText ts ++"]"

outerFold :: [Tree] -> [String]
outerFold ts = L.map toText ts