{-      Coursera NLP programming assignment 2
        CKY Algorithm

        Tanneguy Dulong  05/04/2013

-}

{-      Part 1 main
        data Tree = Leaf String | Node String [Tree]   
            deriving (Show, Eq, Read)

        main = do
        --  t1 <- readFile "dummy.data"
          t1 <- getContents
          putStr $ process t1


        process :: String -> String
        process t = 
                let 
                        lr = listOfTrees t
                        rt = ...
                in unlines rt
-} 


{-      Construction des éléments internes
        Liste de Trees
        Map des termes rares 

        listOfTrees :: String -> [Tree]
        listOfTrees t = 
                let 
                  lst = lines t                                            -- [String]
                  lx1 = L.map treeText lst                           -- [String]
                  lr2 = L.map toTree lx1                              -- [Tree]
                in  lr2
-}

{-      production de la structure Tree à partir d'un fichier texte

        r1 = makeRegex "(\t| )"
        r2 = makeRegex "\\[(\"[^\"]+\"),(\"[^\"]+\")\\]"    -- catches ["DET", "The"]
        r3 = makeRegex "\\[(\"[^\"]+\"),"                     -- catches  ["NP",

        treeText :: String -> String
        treeText t1 =
          let t2 = subRegex r1 t1 ""
            in let t3 = subRegex r2 t2 "Node \\1 [Leaf \\2]"
              in let t4 = subRegex r3 t3 "Node \\1 ["
                in let t5 = subRegex r3 t4 "Node \\1 ["
                  in  t5                                                       

        toTree :: String -> Tree 
        toTree t1 = read t1 :: Tree 
-}


{-      Comptage des éléments rares


        treeLeaves :: Map String Int -> Tree -> Map String Int
        treeLeaves m (Node _  ts) = foldTreeLeaves m ts
        treeLeaves m (Leaf s)        = condInsert m s 

        foldTreeLeaves m []      = m
        foldTreeLeaves m (t:ts) = let m1 = treeLeaves m t 
                                                  in foldTreeLeaves m1 ts

        condInsert :: Map String Int -> String -> Map String Int 
        condInsert m x = if   x == ""
                        then m
                        else M.insertWith (+) x 1 m
-}
{-      Remplacement des éléments rares


        treeRares :: Map String Int -> Tree -> Tree
        treeRares m (Node s  ts) = Node s (L.map (treeRares m) ts)
        treeRares m (Leaf s)  =  if (member s m)  
                                                   then Leaf "_RARE_"
                                                   else Leaf s
-}

{-      Sortie sous forme de texte       

        foldToText :: [Tree] -> String
        foldToText [t] = toText t
        foldToText (t:ts) =  toText t++","++foldToText ts

        toText :: Tree -> String
        toText (Node s [Leaf a]) = "[" ++ show s++  "," ++ show a ++ "]"  
        toText (Node s ts  )        = "[" ++ show s ++ ","++ foldToText ts ++"]"

        outerFold :: [Tree] -> [String]
        outerFold ts = L.map toText ts

-}



{-  Coursera-nlp-1 1st part
  HMM tagger : produit le tag le + probable en fonction 
  des trigrammes de tags et des frequences d'émission

  Usage : tagger_viterbi.exe   rare.e  rare.q gene.test  
-}

-- import Data.Char

import Prelude as P

import Data.Map as M
import Data.Maybe as B
import Data.Char as C
import Data.List as L

import Debug.Trace as D

import System.IO as IO
import System.Environment as E

import Text.JSON as J
import Text.Regex.Base.RegexLike as K 
import Text.Regex as R 
import Text.Printf as T

main = do
    args <- getArgs
    nontermText <- readFile (args !! 0)
    unaryText   <- readFile (args !! 1)
    binaryText  <- readFile (args !! 2)
    text        <- readFile (args !! 3)
    putStr $ process nontermText unaryText binaryText


process :: String -> String -> String -> String
process nontermText unaryText binaryText text = 
        let nt_map = makeMap nontermText                    -- [X] -> Float

            un_map = makeMap unaryText                      -- [X, w] -> Float
            qu_map = M.mapWithKey divParX un_map            -- comptages divisés par nt_map (X)

            bi_map = makeMap binaryText                     -- [X, Y, Z] -> Float
            qb_map = M.mapWithKey divParX bi_map            -- comptages divisés par nt_map (X) 

            br_map = M.foldrWithKey changeMap empty bi_map  -- [Y, Z] -> (String, Float)

            w_list = phr [] (lines text)                 -- [[word, word], [word, word]]

      
        in let ctx = (qu_map, qb_map, ...)
--         res = [tagHmm ctx  (head w_list)]       -- [[word tag\nnword tag\n ...][word tag]]
         res = L.map (tagHmm ctx) w_list           -- [[word tag\nnword tag\n ...][word tag]]
      in unlines res


{-
    max [x,y,z] sur R  et s sur [i..(j-1)]
    max [x,w] sur R'  et s sur [i..(j-1)]
    keys qb_map == énumération sur R

    
    Bottom-up
    
    [1 .. n]   {π(i, i, X)}
    [1 .. n-1]   {π(i,i+1, X)}      {π(i,i+1)}

    [1 .. n-2]   {π(i,i+2, X)}
    π (1, 1, Y) 
    π (2, 2, Z)
    -> rechercher max {X} -> YZ

    le petit chat est stone
    DET ADJ NOUN V ADJ

 data Tree a = Leaf a | Branch (Tree a) a (Tree a)
is as follows
 ana :: (b->Either a (b,a,b)) -> b -> Tree a
 ana unspool x = case unspool x of
                   Left a -> Leaf a
                   Right (l,x,r) -> Branch (ana unspool l) x (ana unspool r)


    π(DET, 1, 1)
    π(NOUN, 2, 2)



    contexte dynamqique 

contexte = [String] mots : phrase à analyser
dynamic = ([Nonterminal_Target])

-}

{-
unaryRule :: Ctx -> Int -> String -> (Float, Maybe Tree)
unaryRule ctx i x = 
    let (s, q, _, _) = ctx
        wds = nub . L.map last $ keys q
        w = s !! i 
    in let  w' = if notElem w wds then "_RARE_" else w
       in   if member [x, w] q 
            then (qu_map ! [x,w] , Just (Node x [Leaf w]) )  
            else (0, Nothing)

unaryAll :: Ctx -> Int -> [(Float, Maybe Tree)]
unaryAll ctx i =
    let (_, qu, _, _) = ctx
        nts = nub . L.map head $ keys q
    in  L.map (unaryRule ctx i) nts 


binaryRule :: Ctx -> Int -> Int -> [String] -> (Float, Maybe Tree)
binaryRule ctx (i,ni) (j,nj) [x,y,z] = 
    let (_, _, q, _) = ctx
    in  if member [x,y,z] q 
        then let score  = qu_map ! [x,y,z] 
                 tree   = Node x [Leaf w]  
        else (0, Nothing)

datatype Res = Maybe (Int, Int, Float, Tree)
binaryRule :: Ctx -> Res -> Res -> [String] -> Res
binaryRule ctx resi resj [x,y,z] = 
    if resi == Nothing || resj == Nothing
    then Nothing
    else let (_, _, q, chlds) = ctx
             (i,s, score_i, tree_i) = fromJust resi
             (s+1,j, score_j, tree_j) = fromJust resj
         in  if member [x,y,z] q 
             then   let score  = qu_map ! [x,y,z] * score_i * score_j 
                        tree   = Node x [tree_i, tree_j]  
                    in Just (i,j,score,tree)
             else Nothing

-}

{-

πs (ctx, i, i, x) = 
    L.map π (ctx, i, i, x) 


π (i, j, x) = maxx (qb_map ! [x,y,z] * π(i, s, y ) * π(s + 1, j, z))
-}

data Res = Res  { pos_i :: Int
                , pos_j :: Int
                , score :: Float
                , tree  :: Tree
                , ctx   :: Ctx
                }
            deriving (Show)


datatype Ctx = Ctx  { sent :: [String],
                    , q_un :: Map [String] Float 
                    , q_bi :: Map [String] Float 
                    , trms  :: [String]
                    , ntrm  :: [String]
                    }

build :: Maybe Res -> Maybe Res -> Maybe Res
build _ Nothing = Nothing
build Nothing _ = Nothing
build is sj = 
    let i = pos_i is
        s = pos_j is
        s'= pos_i sj
        j = pos_j sj
    in if s+1 /= s' 
       then Nothing
       else 


divParX :: [String] -> Float -> Float
divParX key count = 
        let k = take 1 key                      --  on veut une clef de type ["NP"]
            c = nt_map ! k 
        in  count / c
        

mapAdd :: Map [String] Float -> [String] ->  Map [String] Float
mapAdd m entry  =
  let k = drop 2 entry          -- count, type éliminés, on conserve key1,key2,key3
      s = take 1 entry 
      a = read s :: Float       
  in M.insert k a m             


changeMap :: [String] -> Float -> Map [String] (Float, String) -> Map [String] (Float, String) 
changeMap k a m =
  let newk = drop 1 k          -- count, type éliminés, on conserve key1,key2,key3
      tagt = take 1 k 
      newv = (tagt, a)       
  in M.insert newk, newv m             


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




showKV :: [String] -> Float -> String -> String
showKV k a result = result ++ (show k) ++ (show a) ++ "\n" 
showMap m = M.foldrWithKey showKV "" m  
 -}