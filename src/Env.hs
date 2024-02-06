module Env where

import Common (Var, Bond)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)

data Env = Env {
        defs :: [(Var, Bond)],
        currentPrice :: Double,
        currentDate :: Day
    }

initEnv :: IO Env
initEnv = Env [] 0 <$> todayDate

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

findBond :: Env -> Var -> Maybe Bond
findBond [] _ = Nothing
findBond ((v,c):xs) v' = if v == v' then Just c else findBond xs v'