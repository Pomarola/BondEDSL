module Eval where

import Common
import Sugar
import MonadBnd
import PrettyPrinter
import State
import Control.Monad.Cont (MonadIO(liftIO))

eval :: MonadBnd m => Exp -> m Bool
eval (Cashflow (conds, bond)) = do
    printBnd "Cashflow for Bond"
    b <- convertCond conds bond
    case b of
        Just b' -> do 
            printBondCashFlow b'
            return True
        Nothing -> return False
eval (PortCashflow (conds, var)) = do
    printBnd $ "Cashflow for portfolio" ++ var
    p <- lookupPortfolio var 
    case p of 
        Just ps -> do
            bs <- evalPort conds ps 
            printPortfolioCashFlow bs
            return True
        Nothing -> return False

eval _ = return False

evalPort :: MonadBnd m => [Cond] -> [(Int, Var)] -> m [(Var, Bond)]
evalPort _ [] = return []
evalPort conds ((n, v):ps) = do
    b <- lookupDef v
    case b of
        Just b' -> do
            bs <- evalPort conds ps
            bc <- applyConds conds b'
            return ((v, applyScalers (fromIntegral n) bc) : bs)
        Nothing -> return []

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
applyScalers m (At d (Pay r a c)) = At d (Pay (r * m) (a * m) c)

replaceScaler :: Scaler -> Bond -> Bond 
replaceScaler n@(CER new) (Scale (CER old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n@(DolarLinked new) (Scale (DolarLinked old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n (Scale m b) = Scale m (replaceScaler n b)
replaceScaler n (And b1 b2) = And (replaceScaler n b1) (replaceScaler n b2)
replaceScaler _ b@(At _ _) = b 
