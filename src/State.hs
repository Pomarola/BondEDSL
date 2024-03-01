module State (oldDate, todayDate, initState, State(..), Def, Port) where

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (Day)
import Bond (Var, Bond, Money)

type Def = (Var, Bond)
type Port = (Var, [(Int, Var)])

data State = State
  {
    env :: [Def],                   -- Entorno de Bonos
    currentPrice :: Maybe Money,    -- Precio de mercado supuesto
    currentDate :: Day,             -- Fecha supuesta
    porfolios :: [Port]             -- Entorno de Portfolios
  }

todayDate :: IO Day
todayDate = getCurrentTime >>= return . utctDay

oldDate :: Day
oldDate = read "1900-01-01"

initState :: State
initState = State [] Nothing oldDate []