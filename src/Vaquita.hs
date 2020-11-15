{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Vaquita where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.List (partition)
import           Data.Decimal

newtype Participant = Participant { getParticipantName :: String  } deriving (Eq, Ord, Show)
newtype Money       = Money       { getMoney           :: Decimal } deriving (Eq, Ord, Num)
newtype Percentage  = Percentage  { getPercentage      :: Decimal } deriving (Eq, Show, Num)

instance Show Money where
  show (Money m) = "$" ++ show (roundTo 2 m)

type ParticipantsShare a = Map Participant a

data PaidBy = Single Participant
            | Multiple (ParticipantsShare Money)
  deriving (Eq, Show)

data SplitMode = Equally (Set Participant)
               | Unequally (ParticipantsShare Money)
               | ByPercentage (ParticipantsShare Percentage)
  deriving (Eq, Show)

data Expense = Expense
             { expenseDescription :: String
             , expenseAmount      :: Money
             , expensePaidBy      :: PaidBy
             , expenseSplitMode   :: SplitMode
             }
  deriving (Eq, Show)

data WhipRound = WhipRound
               { whipRoundParticipants :: Set Participant
               , whipRoundExpenses     :: [Expense]
               }
  deriving (Eq, Show)

data ExpenseError = EmptyDescription
                  | InvalidAmount
                  | PaidByDoesNotAddUp
                  | SplitEquallyEmptyParticipants
                  | SplitUnequallyDoesNotAddUp
                  | SplitByPercentageDoesNotAddUp
  deriving (Eq, Show)

data SplitDetails = SplitDetails
                  { splitDetsPaid :: Money
                  , splitDetsOwes :: Money
                  }
  deriving (Eq, Show)

data SettleUpTx = SettleUpTx
                  { txFrom   :: Participant
                  , txTo     :: Participant
                  , txAmount :: Money
                  }
  deriving (Eq, Show)

data SettleUp = SettleUp
                { settleUpTotals :: ParticipantsShare Money
                , settleUpTxs    :: [SettleUpTx]
                }
  deriving (Eq, Show)

instance Semigroup SplitDetails where
  (SplitDetails p s) <> (SplitDetails p' s') = SplitDetails (p + p') (s + s')

instance Monoid SplitDetails where
  mempty = SplitDetails 0 0


mkExpense :: String -> Money -> PaidBy -> SplitMode -> Either ExpenseError Expense
mkExpense desc amount paidBy splitMode = do
  desc'      <- if null desc then Left EmptyDescription else Right desc
  amount'    <- if getMoney amount > 0 then Right amount else Left InvalidAmount
  paidBy'    <- validatePaidBy paidBy
  splitMode' <- validateSplitMode splitMode
  pure $ Expense desc' amount' paidBy' splitMode'
 where
  addUp f = sum . map f . Map.elems

  validatePaidBy x@(Single _)    = Right x
  validatePaidBy x@(Multiple ps) =
    if addUp getMoney ps == getMoney amount
      then Right x
      else Left PaidByDoesNotAddUp

  validateSplitMode x@(Equally ps) =
    if null ps then Left SplitEquallyEmptyParticipants else Right x
  validateSplitMode x@(Unequally ps) =
    if addUp getMoney ps == getMoney amount
      then Right x
      else Left SplitUnequallyDoesNotAddUp
  validateSplitMode x@(ByPercentage ps) =
    if addUp getPercentage ps == 100
      then Right x
      else Left SplitByPercentageDoesNotAddUp

mkWhipRound :: Set Participant -> WhipRound
mkWhipRound = flip WhipRound []

addExpense :: WhipRound -> Expense -> WhipRound
addExpense (WhipRound ps es) e = WhipRound ps (e : es)

splitDetails :: Expense -> ParticipantsShare SplitDetails
splitDetails (Expense _ amount paidBy splitMode) =
  uncurry SplitDetails <$> Map.unionWith combine ((,0) <$> paid paidBy) ((0,) <$> split splitMode)
 where
  combine (p, _) (_, s) = (p, s)

  paid (Single p)    = Map.singleton p amount
  paid (Multiple ps) = ps

  split (Equally ps) =
    let eachOwes = Money $ getMoney amount / (fromIntegral . Set.size) ps
    in  Map.fromSet (const eachOwes) ps
  split (Unequally ps)    = ps
  split (ByPercentage ps) = amountPer <$> ps

  amountPer pc = Money $ getMoney amount * getPercentage pc / 100

balances :: WhipRound -> ParticipantsShare SplitDetails
balances = Map.unionsWith (<>) . fmap splitDetails . whipRoundExpenses

settleUp :: WhipRound -> SettleUp
settleUp whipRound =
  let removeSettledUp = filter $ (/= 0.00) . roundTo 2 . getMoney . snd
      totals          = fmap total . balances $ whipRound
      (getBack, owe)  = partition ((> 0) . snd) . removeSettledUp . Map.toList $ totals
      txs'            = txs getBack (fmap negate <$> owe)
  in SettleUp totals txs'
 where
  txs [] _ = []
  txs _ [] = []
  txs ((gbp, gbm):getBack) ((op, om):owe)
    | gbm == om = SettleUpTx op gbp gbm : txs getBack                   owe
    | gbm > om  = SettleUpTx op gbp om  : txs ((gbp, gbm - om):getBack) owe
    | gbm < om  = SettleUpTx op gbp gbm : txs getBack                   ((op, om - gbm):owe)

  total (SplitDetails paid owes) = paid - owes

