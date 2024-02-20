module MonadBnd (
  MonadBnd,
  Bnd,
  runBnd,
  addDef,
  addPortfolio,
  getEnv,
  getPortfolios,
  lookupDef,
  lookupPortfolio,
  setDate,
  getDate,
  setPrice,
  unsetPrice,
  getPrice,
  printBnd,
  printInlineBnd,
  failBnd,
  catchErrors
) where

import Control.Monad.Except ( MonadError(..), ExceptT, runExceptT )
import Control.Monad.State ( MonadIO(..), StateT(runStateT), MonadState(get), gets, modify )
import System.IO ( stderr, hPrint )
import Data.Time.Calendar (Day)

import State
import Bond
import Errors ( Error(..) )

class (MonadIO m, MonadState State m, MonadError Error m) => MonadBnd m where

addDef :: MonadBnd m => Def -> m ()
addDef d = modify (\s -> s { env = d : env s })

addPortfolio :: MonadBnd m => Port -> m ()
addPortfolio p = modify (\s -> s { porfolios = p : porfolios s })

getEnv :: MonadBnd m => m [Def]
getEnv = gets env

getPortfolios :: MonadBnd m => m [Port]
getPortfolios = gets porfolios

lookupVar :: MonadBnd m => (State -> [(Var, a)]) -> Var -> m (Maybe a)
lookupVar f v = do
     s <- get
     case filter (hasName v) (f s) of
       (_,item):_ -> return $ Just item
       [] -> return Nothing
   where hasName :: Var -> (Var, a) -> Bool
         hasName n (n',_) = n == n'

lookupDef :: MonadBnd m => Var -> m (Maybe Bond)
lookupDef = lookupVar env

lookupPortfolio :: MonadBnd m => Var -> m (Maybe [(Int, Var)])
lookupPortfolio = lookupVar porfolios

setDate :: MonadBnd m => Day -> m ()
setDate d = modify (\s-> s { currentDate = d })

getDate :: MonadBnd m => m Day
getDate = gets currentDate

setPrice :: MonadBnd m => Money -> m ()
setPrice b = modify (\s-> s { currentPrice = Just b })

unsetPrice :: MonadBnd m => m ()
unsetPrice = modify (\s-> s { currentPrice = Nothing })

getPrice :: MonadBnd m => m (Maybe Money)
getPrice = gets currentPrice

printBnd :: MonadBnd m => String -> m ()
printBnd = liftIO . putStrLn

printInlineBnd :: MonadBnd m => String -> m ()
printInlineBnd = liftIO . putStr

failBnd :: MonadBnd m => String -> m a
failBnd s = throwError (Error s)

catchErrors  :: MonadBnd m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)

type Bnd = StateT State (ExceptT Error IO)

instance MonadBnd Bnd

runBnd' :: Bnd a -> IO (Either Error (a, State))
runBnd' m = do
  runExceptT $ runStateT m initState

runBnd :: Bnd a -> IO (Either Error a)
runBnd m = fmap fst <$> runBnd' m