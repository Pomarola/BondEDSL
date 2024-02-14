module State where

import Common (Var, Bond)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)

type Def = (Var, Bond)
type Port = (Var, [(Int, Var)])

data State = State
  {
    env :: [Def], -- Entorno con variables globales y su valor
    currentPrice :: Double,
    currentDate :: Day,
    porfolios :: [Port]
  }

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

oldDate :: Day
oldDate = read "1900-01-01"

initState :: State
initState = State [] 0 oldDate []