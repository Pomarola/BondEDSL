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
eval env (Print (conds, bond)) = do
    b <- convertCond env conds bond
    case b of
        Just b' -> do 
            lift $ print b'
            return $ Nothing
        Nothing -> return $ Nothing
eval _ _ = return $ Nothing

