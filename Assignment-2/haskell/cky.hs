{-
   Coursera NLP programming assignment 2
   CKY Algorithm

   Tanneguy Dulong  05/04/2013

-}
import System.IO as IO
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Char as C
import Text.JSON as J
import Text.Regex as R 
import Text.Regex.Base.RegexLike as K 

{-  
          Part 1       Lire et manipuler des arbres binaires (Chomsky Normal Form)
-} 

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


{- 
        Construction des éléments internes
        Liste de Trees
        Map des termes rares 
-}

listOfTrees :: String -> [Tree]
listOfTrees t = 
        let 
          lst = lines t                                            -- [String]
          lx1 = L.map treeText lst                           -- [String]
          lr2 = L.map toTree lx1                              -- [Tree]
        in  lr2

{-
  production de la structure Tree à partir d'un fichier texte
-}

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



{-
    Comptage des éléments rares
-}

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

{- 
        Remplacement des éléments rares
-}

treeRares :: Map String Int -> Tree -> Tree
treeRares m (Node s  ts) = Node s (L.map (treeRares m) ts)
treeRares m (Leaf s)  =  if (member s m)  
                                           then Leaf "_RARE_"
                                           else Leaf s


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

