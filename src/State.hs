module State (oldDate, todayDate, initState, State(..), Def, Port) where

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Bond (Var, Bond, Money)

type Def = (Var, Bond)
type Port = (Var, [(Int, Var)])

data State = State
  {
    env :: [Def], -- Entorno con variables globales y su valor
    currentPrice :: Maybe Money,
    currentDate :: Day,
    porfolios :: [Port]
  }

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

oldDate :: Day
oldDate = read "1900-01-01"

initState :: State
initState = State [] Nothing oldDate []