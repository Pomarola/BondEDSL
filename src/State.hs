module State where

import Common (Var, Bond)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)

type Def = (Var, Bond)

data State = State
  {
    env :: [Def], -- Entorno con variables globales y su valor
    currentPrice :: Double,
    currentDate :: Day
  }

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

initState :: IO State
initState = State [] 0 <$> todayDate