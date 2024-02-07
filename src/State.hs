module State where

import Common (Var, Bond)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)

type Env = [(Var, Bond)]

data State = S
  { inter :: Bool, -- True, si estamos en modo interactivo.
    env :: Env, -- Entorno con variables globales y su valor
    currentPrice :: Double,
    currentDate :: Day
  }

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

initState :: IO State
initState = S False [] 0 <$> todayDate

findBond :: Env -> Var -> Maybe Bond
findBond [] _ = Nothing
findBond ((v,c):xs) v' = if v == v' then Just c else findBond xs v'

