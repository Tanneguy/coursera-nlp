-- import Data.Char
import Prelude as P
import Data.Map as M
import Data.List as L
import Data.Char as C
import System.Environment as E
import System.IO as IO
import Text.Printf as T




{-
   Appel sur 1 fichier  xx.train + seuil -> xx.rare -> xx.emis 
   										 -> xx.counts 
   -> comptage des n-grams

   -> comptage des emissions (avec _RARE_) 
   -> construction des paramètres d'émission

   Sorties
   - format texte pour 
   - en format toList pour les Map
-} 
main = do
    x <- readFile "data/gene.train.counts"
    putStrLn x

{-
dispatch :: [(String, [String] -> IO ())]
dispatch = 	[ ("rarified", rarified)
			, ("",)
			, ("",)
			]
main = do
		(command:args) <- get args
		let (Just action) = lookup command dispatch
		action args
-}