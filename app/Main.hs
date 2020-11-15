module Main (main) where

import           Vaquita
import           Data.Foldable (for_)
import           Data.Either (rights)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  let theGangDoesBBQ = foldl addExpense (mkWhipRound $ Set.fromList theGang) expenses
      SettleUp totals txs = settleUp theGangDoesBBQ
  putStrLn ""
  putStrLn "The Gang Does BBQ:"
  putStrLn ""
  putStrLn "Totals:"
  for_ (Map.mapWithKey formatTotal totals) putStrLn
  putStrLn ""
  putStrLn "Txs:"
  for_ txs (putStrLn . formatTx)
 where
  formatTotal (Participant name) total =
    name ++ " " ++ (if total > 0 then "gets back " else "owes ") ++ show (abs total)
  formatTx (SettleUpTx from to amount) =
    getParticipantName from ++ " pays " ++ show amount ++ " to " ++ getParticipantName to


dennis  = Participant "Dennis"
charlie = Participant "Charlie"
mac     = Participant "Mac"
dee     = Participant "Dee"
frank   = Participant "Frank"

theGang = [dennis, charlie, mac, dee, frank]

mkExpense' d a p s = mkExpense d (Money a) (Single p) (Equally $ Set.fromList s)

expenses = rights
  [ mkExpense' "Burgers"  45 dennis  theGang
  , mkExpense' "Sausages" 30 dennis  theGang
  , mkExpense' "Veggies"  18 mac     [dennis, mac, dee]
  , mkExpense' "Chees"     7 charlie [charlie, frank]
  , mkExpense' "Ham"      73 frank   [charlie, frank]
  , mkExpense' "Booze"  3700 frank   theGang
  ]


