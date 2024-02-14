module Common where

import Prelude hiding (and)
import Data.Time.Calendar (Day)
import Data.List (sortBy)
import Data.Function (on)

type Var = String

data Currency = USD | ARS | EUR | BTC | ETH | None-- por ahi deberia hacer un tipe check para que no se puedan sumar cosas de distintas monedas
    deriving (Eq, Show)

data Frequency = Annual | SemiAnnual | Quarterly | Monthly
    deriving (Eq, Show)

data Scaler = Mult Double | CER Double | DolarLinked Double
    deriving Show

data Cond = BCCER Double | BCTC Double | Date Day | CV Double | VN Int | Today               -- CV seria current value, date el dia que queremos sup, bccer cer acutal y bctc tipocambio actual
    deriving Show

data DefOrExp = Def Var SugarBond | Eval Exp
    deriving Show

data Exp =
    Print CondBond      -- Uno que printee el contrato y otro que printee el cash flow?
    | Tir CondBond
    | Yield CondBond
    | Price CondBond
    deriving Show

type CondBond = ([Cond], SugarBond)

data Payment = PZero | Pay Double Double Currency
    deriving Show

data SugarBond =
    SVar Var
    | SAnd SugarBond SugarBond
    | SScale Scaler SugarBond
    | SAt Day Payment
    | SRepeat Int Frequency Day Payment
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

bondAsList :: Bond -> [(Day, Double, Double, Currency)]
bondAsList (At d PZero) = [(d, 0, 0, None)]
bondAsList (At d (Pay a r c)) = [(d, a, r, c)]
bondAsList (And b1 b2) = bondAsList b1 ++ bondAsList b2
bondAsList (Scale _ b) = bondAsList b

sortByDay :: [(Day, Double, Double, Currency)] -> [(Day, Double, Double, Currency)]
sortByDay = sortBy (compare `on` (\(d, _, _, _) -> d))

filterFrom :: Day -> [(Day, Double, Double, Currency)] -> [(Day, Double, Double, Currency)]
filterFrom d = filter (\(d', _, _, _) -> d' >= d)
