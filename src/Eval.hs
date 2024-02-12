module Eval where

import Common
import Sugar
import State
import           System.Console.Haskeline
import           Control.Monad.Except
import MonadBnd




-- eval :: Exp -> Env -> Result
-- eval (op c) = case op of
--     Print -> print c
--     Tir -> tir c
--     Yield -> yield c
--     Price -> price c

-- eval :: Env -> Exp -> InputT IO (Maybe a)
-- eval env (Print (conds, bond)) = do
--     b <- convertCond env conds bond
--     case b of
--         Just b' -> do 
--             lift $ print b'
--             return $ Nothing
--         Nothing -> return $ Nothing
-- eval _ _ = return $ Nothing

eval :: MonadBnd m => Exp -> m (Maybe Bond)
eval _ = return Nothing

-- convertCond :: Env -> [Cond] -> SugarBond -> InputT IO (Maybe Bond)
-- convertCond env conds sb = do
--     b <- convert env sb
--     case b of
--         Just b' -> do
--             cb <- applyConds conds b' 
--             return $ Just cb
--         Nothing -> return $ Nothing

-- applyConds :: [Cond] -> Bond -> Bond
-- applyConds [] b = b
-- applyConds ((BCCER cer):cs) b = let b' = replaceScaler (CER cer) b in applyConds cs b'
-- applyConds ((BCTC tc):cs) b = let b' = replaceScaler (DolarLinked tc) b in applyConds cs b'
-- applyConds ((VN n):cs) b = applyConds cs (Scale (Mult (fromIntegral n)) b)
-- applyConds (_:xs) b = applyConds xs b

-- applyScalers :: Double -> Bond -> Bond 
-- applyScalers m (Scale (Mult s) b) = applyScalers (m * s) b
-- applyScalers m (Scale s@(CER _) b) = Scale s (applyScalers m b)
-- applyScalers m (Scale s@(DolarLinked _) b) = Scale s (applyScalers m b)
-- applyScalers m (And b1 b2) = And (applyScalers m b1) (applyScalers m b2)
-- applyScalers _ b@(At _ PZero) = b
-- applyScalers m (At d (Pay (a, c1) (b, c2))) = At d (Pay (a * m, c1) (b * m, c2))

-- replaceScaler :: Scaler -> Bond -> Bond 
-- replaceScaler n@(CER new) (Scale (CER old) b) = Scale (Mult (new / old)) (replaceScaler n b)
-- replaceScaler n@(DolarLinked new) (Scale (DolarLinked old) b) = Scale (Mult (new / old)) (replaceScaler n b)
-- replaceScaler n (Scale m b) = Scale m (replaceScaler n b)
-- replaceScaler n (And b1 b2) = And (replaceScaler n b1) (replaceScaler n b2)
-- replaceScaler _ b@(At _ _) = b 
