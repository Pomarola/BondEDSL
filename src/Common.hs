module Common where

import Prelude hiding (and)
-- import Data.Time
-- import Data.Dates

data Currency = USD | ARS | EUR | BTC | ETH
    deriving (Eq, Show)

data Date = Date Int Int Int deriving (Eq, Show)

data Frequency = Annual | Semestral | Trimestral | Mensual
    deriving (Eq, Show)

type Yield = Double

-- type Dues = Int
-- type Amount = Double
-- type Rate = Double

-- type Interval = (Date, Date, Int)

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
    | SAt Date SugarContract
    | SZcb Scaler Currency Date
    deriving Show

data Contract =
    Var Var
    | Zero
    | One Currency
    | And Contract Contract
    | Or Contract Contract
    | Give Contract
    | Scale Scaler Contract
    | At Date Contract
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

at :: Date -> Contract -> Contract
at = At

-- prestamo :: Date -> Currency -> Amount -> Rate -> Contract
-- prestamo d c a r =  let take = payment d a c
--                         give = payment d (a * r) c
--                   in take `andGive` give


-- `and ` zcb (date "1200GMT 10 Dec 2020") 20 GBP 
-- `and ` zcb (date "1200GMT 10 May 2021") 20 GBP 
-- `and ` zcb (date "1200GMT 10 Dec 2021") 1020 GBP
-- `and ` give (zcb (date "1200GMT 1Apr 2020") 1000 GBP))

-- al30 :: Contract
-- al30 = payment (mkDate "10 Dec 2020") 20 GBP `and` 

-- couponBond :: Date -> Date -> Dues -> Double -> Currency -> Contract

-- mkDate :: String -> Date 

-- andGive :: Contract -> Contract -> Contract
-- andGive c1 c2 = and c1 (give c2)


-- payment :: Date -> Amount -> Currency -> Contract 
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