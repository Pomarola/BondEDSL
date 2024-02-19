module Sugar (convert) where

import Data.Time.Calendar (Day, addGregorianMonthsClip)
import Prelude hiding (and, repeat)

import Common
import MonadBnd

repeat :: Int -> Frequency -> Day -> Payment -> Bond
repeat 1 _ d p = at d p
repeat n f d p = at d p `and` repeat (n-1) f (nextDate f 1 d) p

nextDate :: Frequency -> Int -> Day -> Day
nextDate Annual i d = addGregorianMonthsClip (toInteger $ 12 * i) d
nextDate SemiAnnual i d = addGregorianMonthsClip (toInteger $ 6 * i) d
nextDate Quarterly i d = addGregorianMonthsClip (toInteger $ 3 * i) d
nextDate Monthly i d = addGregorianMonthsClip (toInteger $ 1 * i) d

getInterest :: Frequency -> Double -> Double
getInterest Annual r = r
getInterest SemiAnnual r = r / 2
getInterest Quarterly r = r / 4
getInterest Monthly r = r / 12

couponBullet :: Int -> Frequency -> Day -> Double -> Double -> Currency -> Bond
couponBullet n f d r b c = let rent = (getInterest f r / 100 * b) 
                            in repeat (n - 1) f d (pay rent 0 c) `and` at (nextDate f (n - 1) d) (pay rent b c)

couponAmort :: Int -> Frequency -> Day -> Double -> Double -> Double -> Currency -> Bond
couponAmort 1 f d r a b c = let rent = (getInterest f r / 100 * b) 
                            in at d (pay rent a c)
couponAmort n f d r a b c = let rent = (getInterest f r / 100 * b) 
                            in at d (pay rent a c) `and` couponAmort (n - 1) f (nextDate f 1 d) r a (b - a) c

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
convert (SRepeat n f d p) = return $ Just (repeat n f d p)
convert (SCouponAmort n f d r a b c) = return $ Just (couponAmort n f d r a b c)
convert (SCouponBullet n f d r b c) = return $ Just (couponBullet n f d r b c)