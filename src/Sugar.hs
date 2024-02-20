module Sugar where

import Data.Time.Calendar (Day, addGregorianMonthsClip)
import Prelude hiding (and, repeat)

import Bond
import MonadBnd

data Frequency = Annual | SemiAnnual | Quarterly | Monthly
    deriving (Eq, Show)

data Cond = BCCER Double | BCTC Double | Date Day | CV Double Currency | VN Int | Today               -- CV seria current value, date el dia que queremos sup, bccer cer acutal y bctc tipocambio actual
    deriving Show

data DefOrExp = Def Var SugarBond | Eval Exp | Portfolio Var [(Int, Var)]
    deriving Show

data Exp =
    Print CondBond      -- Uno que printee el contrato y otro que printee el cash flow?
    | Yield CondBond
    | Parity CondBond
    | Detail CondBond
    | Cashflow CondBond
    | PortCashflow ([Cond], Var)
    deriving Show

type CondBond = ([Cond], SugarBond)

type Iterator = (Int, Frequency, Day)

data SugarBond =
    SVar Var
    | SAnd SugarBond SugarBond
    | SScale Scaler SugarBond
    | SAt Day Payment
    | SRepeat Iterator Payment
    | SCouponBullet Iterator Double Money
    | SCouponAmort Iterator Double Double Money
    deriving Show

repeat :: Int -> Frequency -> Day -> Payment -> Bond
repeat 1 _ d p = at d p
repeat n f d p = at d p `and` repeat (n-1) f (calcDate f 1 d) p

calcDate :: Frequency -> Int -> Day -> Day
calcDate Annual i d = addGregorianMonthsClip (toInteger $ 12 * i) d
calcDate SemiAnnual i d = addGregorianMonthsClip (toInteger $ 6 * i) d
calcDate Quarterly i d = addGregorianMonthsClip (toInteger $ 3 * i) d
calcDate Monthly i d = addGregorianMonthsClip (toInteger $ 1 * i) d

getRate :: Frequency -> Double -> Double
getRate Annual r = r
getRate SemiAnnual r = r / 2
getRate Quarterly r = r / 4
getRate Monthly r = r / 12

couponBullet :: Int -> Frequency -> Day -> Double -> Double -> Currency -> Bond
couponBullet n f d r b c = let rent = (getRate f r / 100 * b) 
                            in repeat (n - 1) f d (pay rent 0 c) `and` at (calcDate f (n - 1) d) (pay rent b c)

couponAmort :: Int -> Frequency -> Day -> Double -> Double -> Double -> Currency -> Bond
couponAmort 1 f d r a b c = let rent = (getRate f r / 100 * b) 
                            in at d (pay rent a c)
couponAmort n f d r a b c = let rent = (getRate f r / 100 * b) 
                            in at d (pay rent a c) `and` couponAmort (n - 1) f (calcDate f 1 d) r a (b - a) c

convert :: MonadBnd m => SugarBond -> m (Maybe Bond)
convert (SVar v) = lookupDef v
convert (SAnd b1 b2) = do
    b1' <- convert b1
    case b1' of
        Just b1'' -> do
            b2' <- convert b2
            case b2' of
                Just b2'' -> return $ Just (and b1'' b2'')
                Nothing -> return Nothing
        Nothing -> return Nothing
convert (SScale s b) = do
    b' <- convert b
    case b' of
        Just b'' -> return $ Just (scale s b'')
        Nothing -> return Nothing
convert (SAt d p) = return $ Just (at d p)
convert (SRepeat (0, _, _) _) = return Nothing
convert (SRepeat (n, f, d) p) = return $ Just (repeat n f d p)
convert (SCouponBullet (0, _, _) _ _) = return Nothing
convert (SCouponBullet (n, f, d) r (b, c)) = return $ Just (couponBullet n f d r b c)
convert (SCouponAmort (0, _, _) _ _ _) = return Nothing
convert (SCouponAmort (n, f, d) r a (b, c)) = return $ Just (couponAmort n f d r a b c)