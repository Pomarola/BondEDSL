module Bond where

import Prelude hiding (and)
import Data.Time.Calendar (Day)
import Data.List (sortBy)
import Data.Function (on)

type Var = String
type Currency = String
type Money = (Double, Currency)

data Scaler = Mult Double | CER Double | DolarLinked Double
    deriving Show

data Payment = PZero | Pay Double Double Currency
    deriving Show

data Bond =
    And Bond Bond
    | Scale Scaler Bond
    | At Day Payment
    deriving Show

and :: Bond -> Bond -> Bond
and = And

scale :: Scaler -> Bond -> Bond
scale = Scale

at :: Day -> Payment -> Bond
at = At

pay :: Double -> Double -> Currency -> Payment
pay = Pay

type BondAsTuple = (Day, Maybe Var, Double, Double, Currency, [Scaler])

bondAsList :: Maybe Var -> Bond -> [BondAsTuple]
bondAsList Nothing = bondAsList' [] Nothing
bondAsList v = bondAsList' [] v

bondAsList' :: [Scaler] -> Maybe Var -> Bond -> [BondAsTuple]
bondAsList' xs v (At d PZero) = [(d, v, 0, 0, "N/A", xs)]
bondAsList' xs v (At d (Pay r a c)) = [(d, v, r, a, c, xs)]
bondAsList' xs v (And b1 b2) = bondAsList' xs v b1 ++ bondAsList' xs v b2
bondAsList' xs v (Scale s b) = bondAsList' (s : xs) v b 

sortByDay :: [BondAsTuple] -> [BondAsTuple]
sortByDay = sortBy (compare `on` tupleDate)

filterFrom :: Day -> [BondAsTuple] -> [BondAsTuple]
filterFrom d = filter (\t -> tupleDate t > d)

filterTo :: Day -> [BondAsTuple] -> [BondAsTuple]
filterTo d = filter (\t -> tupleDate t <= d)

tupleDate :: BondAsTuple -> Day
tupleDate (d, _, _, _, _, _) = d