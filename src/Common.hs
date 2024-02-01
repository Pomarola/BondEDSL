module Common where

import Prelude hiding (and)
import Data.Time.Calendar (Day)

type Var = String
type Env = [(Var, Contract)]

data Currency = USD | ARS | EUR | BTC | ETH -- por ahi deberia hacer un tipe check para que no se puedan sumar cosas de distintas monedas
    deriving (Eq, Show)

data Frequency = Annual | SemiAnnual | Quarterly | Monthly
    deriving (Eq, Show)

data Scaler = Mult Double | CER Double | DolarLinked Double
    deriving Show

data Cond = BCCER Double | BCTC Double | Date Day | CV Double                -- CV seria current value, date el dia que queremos sup, bccer cer acutal y bctc tipocambio actual
    deriving Show

data DefOrExp = Def Var SugarContract | Eval Exp
    deriving Show

data Exp =
    Print CondContract
    | Tir CondContract
    | Yield CondContract
    | Price CondContract
    deriving Show

data CondContract = CC Cond SugarContract | SC SugarContract
    deriving Show

data SugarContract =
    SVar Var
    | SZero
    | SOne Currency
    | SAnd SugarContract SugarContract
    | SOr SugarContract SugarContract
    | SScale Scaler SugarContract
    | SAt Day SugarContract
    | SZcb Scaler Currency Day
    | SPay Scaler Currency
    | SRepeat Int Frequency Day SugarContract
    deriving Show

data Contract =
    Zero
    | One Currency
    | And Contract Contract
    | Or Contract Contract
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

scale :: Scaler -> Contract -> Contract
scale = Scale

at :: Day -> Contract -> Contract
at = At