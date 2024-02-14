module MonadBnd where

import State
import Common
import Errors
import Control.Monad.Except
import Control.Monad.State hiding (State)
import System.IO

import Data.Time.Calendar (Day)

class (MonadIO m, MonadState State m, MonadError Error m) => MonadBnd m where

addDef :: MonadBnd m => Def -> m ()
addDef d = modify (\s -> s { env = d : env s })

lookupDef :: MonadBnd m => Var -> m (Maybe Bond)
lookupDef v = do
     s <- get
     case filter (hasName v) (env s) of
       (_,bond):_ -> return $ Just bond
       [] -> return Nothing
   where hasName :: Var -> Def -> Bool
         hasName n (n',_) = n == n'

setDate :: MonadBnd m => Day -> m ()
setDate d = modify (\s-> s { currentDate = d })

getDate :: MonadBnd m => m Day
getDate = gets currentDate

setPrice :: MonadBnd m => Double -> m ()
setPrice b = modify (\s-> s { currentPrice = b })

getPrice :: MonadBnd m => m Double
getPrice = gets currentPrice

printBnd :: MonadBnd m => String -> m ()
printBnd = liftIO . putStrLn

printInlineBnd :: MonadBnd m => String -> m ()
printInlineBnd = liftIO . putStr

getEnv :: MonadBnd m => m [Def]
getEnv = gets env

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