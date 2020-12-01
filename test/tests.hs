{-# LANGUAGE TemplateHaskell #-}

module Main where

import Universum
import           Hedgehog
import           Hedgehog.Main (defaultMain)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Decimal

import           Vaquita

main :: IO ()
main = defaultMain [tests]

tests :: IO Bool
tests = checkParallel $$(discover)

-- Properties --
-- ---------- --

prop_mkExpense :: Property
prop_mkExpense =
  property $ do
    desc      <- forAll $ Gen.text (Range.linear 1 100) Gen.alpha
    amount    <- forAll $ moneyGen 0.10 1000.00
    paidBy    <- forAll $ paidByGen amount
    splitMode <- forAll $ splitModeGen amount
    let res = mkExpense desc amount paidBy splitMode
    annotateShow res
    assert $ isRight res
    let (Right (Expense desc' amount' paidBy' splitMode')) = res
    desc      === desc'
    amount    === amount'
    paidBy    === paidBy'
    splitMode === splitMode'

prop_mkExpenseInvalid :: Property
prop_mkExpenseInvalid =
  property $ do
    (desc, amount, paidBy, splitMode, error) <- forAll invalidExpenseGen
    mkExpense desc amount paidBy splitMode  === Left error

prop_splitDetailsPaidAndOwedEqualsAmount :: Property
prop_splitDetailsPaidAndOwedEqualsAmount =
  property $ do
    expense <- forAll expenseGen
    let dets = splitDetails expense
        paid = roundTo 2 $ sum (getMoney . splitDetsPaid <$> dets)
        owed = roundTo 2 $ sum (getMoney . splitDetsOwes <$> dets)
    getMoney (expenseAmount expense) === paid
    getMoney (expenseAmount expense) === owed

prop_balancesPaidAndOwedCancelEachOtherOut :: Property
prop_balancesPaidAndOwedCancelEachOtherOut =
  property $ do
    whipRound <- forAll whipRoundGen
    let balances' = balances whipRound
        SplitDetails paid owed = fold balances'
        paid' = roundTo 2 $ getMoney paid
        owed' = roundTo 2 $ getMoney owed
    paid' === owed'

prop_settleUpTotalsAndSettleUpsAddUp :: Property
prop_settleUpTotalsAndSettleUpsAddUp =
  property $ do
    whipRound <- forAll whipRoundGen
    let toBalances (SettleUpTx from to amount) = Map.fromList [(from, -amount), (to, amount)]
        settleUp' = settleUp whipRound
        totals'   = Map.unionsWith (+) . fmap toBalances . settleUpTxs $ settleUp'
        round     = fmap $ roundTo 2 . getMoney
    annotateShow settleUp'
    Map.filter (/= 0.00) (round $ settleUpTotals settleUp') === round totals'

-- Generators --
-- ---------- --

jeff    = Participant "Jeff"
britta  = Participant "Britta"
troy    = Participant "Troy"
abed    = Participant "Abed"
annie   = Participant "Annie"
pierce  = Participant "Pierce"
shirley = Participant "Shirley"

participants = Set.fromList [jeff, britta, troy, abed, annie, pierce, shirley]

participantGen :: Gen Participant
participantGen = Gen.element . Set.toList $ participants

decimalGen :: Decimal -> Decimal -> Gen Decimal
decimalGen min max = Gen.realFrac_ $ Range.constant min max

moneyGen :: Decimal -> Decimal -> Gen Money
moneyGen min max = Money . roundTo 2 <$> decimalGen min max

splitDecimalGen :: Int -> Decimal -> Gen [Decimal]
splitDecimalGen 1 d = pure [d]
splitDecimalGen n d = do
  d' <- decimalGen 0.10 d
  (d' :) <$> splitDecimalGen (n - 1) (d - d')

paidByGen :: Money -> Gen PaidBy
paidByGen amount = Gen.choice [paidBySingleGen, paidByMultipleGen amount]

paidBySingleGen :: Gen PaidBy
paidBySingleGen = Single <$> participantGen

paidByMultipleGen :: Money -> Gen PaidBy
paidByMultipleGen (Money totalAmount) = do
  payers <- Gen.set (Range.constant 2 $ length participants) participantGen
  share  <- splitDecimalGen (length payers) totalAmount
  pure . Multiple . Map.fromList . zip (Set.toList payers) . fmap Money $ share

splitModeGen :: Money -> Gen SplitMode
splitModeGen amount = Gen.choice [splitModeEquallyGen, splitModeUnequallyGen amount, splitModeByPercentageGen]

splitModeEquallyGen :: Gen SplitMode
splitModeEquallyGen = do
  participants <- Gen.set (Range.constant 1 $ length participants) participantGen
  pure $ Equally participants

splitModeUnequallyGen :: Money -> Gen SplitMode
splitModeUnequallyGen (Money totalAmount) = do
  peeps <- Gen.set (Range.constant 1 $ length participants) participantGen
  share <- splitDecimalGen (length peeps) totalAmount
  pure . Unequally . Map.fromList . zip (Set.toList peeps) . fmap Money $ share

splitModeByPercentageGen :: Gen SplitMode
splitModeByPercentageGen = do
  peeps <- Gen.set (Range.constant 1 $ length participants) participantGen
  share <- splitDecimalGen (length peeps) 100.00
  pure . ByPercentage . Map.fromList . zip (Set.toList peeps) . fmap Percentage $ share

invalidExpenseGen :: Gen (Text, Money, PaidBy, SplitMode, ExpenseError)
invalidExpenseGen = do
  desc      <- Gen.text (Range.linear 1 100) Gen.alpha
  amount    <- moneyGen 0.10 1000.00
  paidBy    <- paidByMultipleGen amount
  splitMode <- splitModeGen amount
  makeInvalid <- Gen.element [invalidDesc, invalidAmount, invalidPaidBy, invalidSplitMode]
  makeInvalid desc amount paidBy splitMode
 where
  invalidDesc _ a pb sm =
    pure ("", a, pb, sm, EmptyDescription)
  invalidAmount d _ pb sm = do
    amount <- decimalGen 0.00 1000.00
    pure (d, Money (-amount), pb, sm, InvalidAmount)
  invalidPaidBy d a (Multiple m) sm =
    pure (d, a, Multiple $ Map.map (\(Money d) -> Money $ d + 0.1) m, sm, PaidByDoesNotAddUp)
  invalidSplitMode d a pb (Equally _) =
    pure (d, a, pb, Equally Set.empty, SplitEquallyEmptyParticipants)
  invalidSplitMode d a pb (Unequally m) =
    pure (d, a, pb, Unequally $ Map.map (\(Money d) -> Money $ d + 0.1) m, SplitUnequallyDoesNotAddUp)
  invalidSplitMode d a pb (ByPercentage m) =
    pure (d, a, pb, ByPercentage $ Map.map (\(Percentage d) -> Percentage $ d + 0.1) m, SplitByPercentageDoesNotAddUp)

expenseGen :: Gen Expense
expenseGen = do
  desc      <- Gen.text (Range.linear 1 100) Gen.alpha
  amount    <- moneyGen 0.10 1000.00
  paidBy    <- paidByGen amount
  splitMode <- splitModeGen amount
  let (Right expense) = mkExpense desc amount paidBy splitMode
  pure expense

whipRoundGen :: Gen WhipRound
whipRoundGen = do
  expenses <- Gen.list (Range.linear 1 50) expenseGen
  pure $ WhipRound participants expenses

