module Eval where

import Common
import Sugar
import           System.Console.Haskeline
import           Control.Monad.Except



-- eval :: Exp -> Env -> Result
-- eval (op c) = case op of
--     Print -> print c
--     Tir -> tir c
--     Yield -> yield c
--     Price -> price c

eval :: Env -> Exp -> InputT IO (Maybe a)
eval env (Print c) = do
    c' <- convert env c
    case c' of
        Just c'' -> do 
            lift $ print c''
            return $ Nothing
        Nothing -> return $ Nothing
eval _ _ = return $ Nothing

