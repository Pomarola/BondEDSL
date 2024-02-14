module Eval where

import Common
import Sugar
import MonadBnd
import PrettyPrinter
import State
import Control.Monad.Cont (MonadIO(liftIO))

eval :: MonadBnd m => Exp -> m (Maybe Bond)
eval (Print (conds, bond)) = do
    b <- convertCond conds bond
    case b of
        Just b' -> do 
            printBnd (show b')
            printBondCashFlow b'
            return Nothing
        Nothing -> return Nothing
eval _ = return Nothing

convertCond :: MonadBnd m => [Cond] -> SugarBond -> m (Maybe Bond)
convertCond conds sb = do
    b <- convert sb
    case b of
        Just b' -> do
            cb <- applyConds conds b' 
            return $ Just (evalScalers cb)
        Nothing -> return Nothing

applyConds :: MonadBnd m => [Cond] -> Bond -> m Bond
applyConds [] b = return b
applyConds ((BCCER cer):cs) b = let b' = replaceScaler (CER cer) b in applyConds cs b'
applyConds ((BCTC tc):cs) b = let b' = replaceScaler (DolarLinked tc) b in applyConds cs b'
applyConds ((VN n):cs) b = applyConds cs (Scale (Mult (fromIntegral n)) b)
applyConds ((CV v):cs) b = do
    setPrice v
    applyConds cs b
applyConds ((Date d):cs) b = do
    setDate d
    applyConds cs b
applyConds (Today:cs) b = do
    d <- liftIO todayDate
    setDate d
    applyConds cs b

evalScalers :: Bond -> Bond
evalScalers = applyScalers 1

applyScalers :: Double -> Bond -> Bond 
applyScalers m (Scale (Mult s) b) = applyScalers (m * s) b
applyScalers m (Scale s@(CER _) b) = Scale s (applyScalers m b)
applyScalers m (Scale s@(DolarLinked _) b) = Scale s (applyScalers m b)
applyScalers m (And b1 b2) = And (applyScalers m b1) (applyScalers m b2)
applyScalers _ b@(At _ PZero) = b
applyScalers m (At d (Pay (a, c1) (b, c2))) = At d (Pay (a * m, c1) (b * m, c2))

replaceScaler :: Scaler -> Bond -> Bond 
replaceScaler n@(CER new) (Scale (CER old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n@(DolarLinked new) (Scale (DolarLinked old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n (Scale m b) = Scale m (replaceScaler n b)
replaceScaler n (And b1 b2) = And (replaceScaler n b1) (replaceScaler n b2)
replaceScaler _ b@(At _ _) = b 
