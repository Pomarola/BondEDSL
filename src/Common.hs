module Common where

import Prelude hiding (and)
import Data.Time.Calendar (Day)
-- import Data.Days

data Currency = USD | ARS | EUR | BTC | ETH
    deriving (Eq, Show)

-- data Day = Day Int Int Int deriving (Eq, Show)

data Frequency = Annual | SemiAnnual | Quarterly | Monthly
    deriving (Eq, Show)

type Yield = Double

-- type Dues = Int
-- type Amount = Double
-- type Rate = Double

-- type Interval = (Day, Day, Int)

data Scaler = Mult Double | CER | DolarLinked
    deriving Show

data DefOrExp = Def Var SugarContract | Eval Exp
    deriving Show

data Exp =
    Print SugarContract
    | Tir SugarContract
    | Yield SugarContract
    | Price SugarContract
    deriving Show

type Env = [(Var, Contract)]

type Var = String

data SugarContract =
    SVar Var
    | SZero
    | SOne Currency
    | SAnd SugarContract SugarContract
    | SOr SugarContract SugarContract
    | SGive SugarContract
    | SScale Scaler SugarContract
    | SAt Day SugarContract
    | SZcb Scaler Currency Day
    | SPay Scaler Currency
    | SRepeat Int Frequency Day SugarContract
    deriving Show

data Contract =
    Var Var
    | Zero
    | One Currency
    | And Contract Contract
    | Or Contract Contract
    | Give Contract
    | Scale Scaler Contract
    | At Day Contract
    deriving Show

zero :: Contract
zero = Zero

one :: Currency -> Contract
one = One

and :: Contract -> Contract -> Contract
and = And

or :: Contract -> Contract -> Contract
or = Or

give :: Contract -> Contract
give = Give

scale :: Scaler -> Contract -> Contract
scale = Scale

at :: Day -> Contract -> Contract
at = At

-- prestamo :: Day -> Currency -> Amount -> Rate -> Contract
-- prestamo d c a r =  let take = payment d a c
--                         give = payment d (a * r) c
--                   in take `andGive` give


-- `and ` zcb (Day "1200GMT 10 Dec 2020") 20 GBP 
-- `and ` zcb (Day "1200GMT 10 May 2021") 20 GBP 
-- `and ` zcb (Day "1200GMT 10 Dec 2021") 1020 GBP
-- `and ` give (zcb (Day "1200GMT 1Apr 2020") 1000 GBP))

-- al30 :: Contract
-- al30 = payment (mkDay "10 Dec 2020") 20 GBP `and` 

-- couponBond :: Day -> Day -> Dues -> Double -> Currency -> Contract

-- mkDay :: String -> Day 

-- andGive :: Contract -> Contract -> Contract
-- andGive c1 c2 = and c1 (give c2)


-- payment :: Day -> Amount -> Currency -> Contract 
-- payment d a c = at d (scale a (one c))

-- instance Num a => Num (Obs a) where
--    fromInteger i = konst (fromInteger i)
--    (+) = liftA2 (+)
--    (-) = liftA2 (-)
--    (*) = liftA2 (*)
--    abs = fmap abs
--    signum = fmap signum

-- (%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
-- (%<)  = liftA2 (<)
-- (%<=) = liftA2 (<=)
-- (%=)  = liftA2 (==)
-- (%>=) = liftA2 (>=)
-- (%>)  = liftA2 (>)