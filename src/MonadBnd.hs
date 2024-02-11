module MonadBnd where

import State
import Common
import Error ( Error(..) )
import Control.Monad.Except
import Control.Monad.State

class (MonadIO m, MonadState State m, MonadError Error m) => MonadBnd m where

setInter :: MonadBnd m => Bool -> m ()
setInter b = modify (\s-> s { inter = b })

getInter :: MonadBnd m => m Bool
getInter = gets inter

addDef :: MonadBnd m => Def -> m ()
addDef d = modify (\s -> s { env = d : env s })

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

lookupDef :: MonadBnd m => Var -> m (Maybe Bond)
lookupDef v = do
     s <- get
     case filter (hasName v) (env s) of
       (_,bond):_ -> return (Just bond)
       [] -> return Nothing
   where hasName :: Name -> Def -> Bool
         hasName v (v',_) = v == v'

getEnv :: MonadBnd m => m [Def]
getEnv = gets env

failBnd :: MonadBnd m => String -> m a
failBnd s = throwError (Error s)

type Bnd = StateT State (ExceptT Error IO)

instance MonadBnd Bnd

runBnd' :: Bnd a -> IO (Either Error (a, State))
runBnd' m =  runExceptT $ runStateT m initState

runBnd :: Bnd a -> IO (Either Error a)
runBnd m = fmap fst <$> runBnd' m