module Common where

import Prelude hiding (and)
-- import Data.Time
-- import Data.Dates

data Currency = USD | ARS | EUR | BTC | ETH
    deriving (Eq, Show)

data Date = Date Int Int Int deriving (Eq, Show)


-- type Dues = Int
-- type Amount = Double
-- type Rate = Double

-- type Interval = (Date, Date, Int)

-- data DefOrExp 

-- data Env = [(Var, Contract)]
--     deriving Show

-- data DefOrExp = Def Var Contract | Eval Operation Contract
--     deriving Show
    


-- data Operation = TIR | Yield | Price
--     deriving Show

type Env = [Def]

type Var = String

data Def = Def Var Contract
    deriving Show

data Contract =
    Var Var
    | Zero 
    | One Currency
    | And Contract Contract
    | Or Contract Contract
    | Give Contract
    | Scale Double Contract
    | At Date Contract
    deriving Show
    
zero :: Contract
zero = Zero

one :: Currency -> Contract 
one c = One c

and :: Contract -> Contract -> Contract
and c1 c2 = And c1 c2

or :: Contract -> Contract -> Contract
or c1 c2 = Or c1 c2

give :: Contract -> Contract
give c = Give c

scale :: Double -> Contract -> Contract
scale x c = Scale x c

at :: Date -> Contract -> Contract
at d c = At d c

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