module Eval (eval) where

import Control.Monad.Cont (MonadIO(liftIO))
import Data.Time.Calendar (Day, diffDays)
import Bond
import Sugar
import MonadBnd
import PrettyPrinter
import State

eval :: MonadBnd m => Exp -> m Bool
eval (Cashflow (conds, bond)) = do
    case bond of
        SVar name -> printBnd ("Cashflow for Bond " ++ name) 
        _ -> printBnd "Cashflow"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            from <- getDate
            let cf = sortByDay $ filterFrom from $ bondAsList Nothing b'
            printBondCashFlow cf
            return True
        Nothing -> return False

eval (PortCashflow (conds, var)) = do
    printBnd $ "Cashflow for portfolio " ++ var
    p <- lookupPortfolio var
    case p of
        Just ps -> do
            ps' <- evalPort conds ps
            from <- getDate
            let cfs = sortByDay $ filterFrom from $ concatMap (\(v,b) -> bondAsList (Just v) b) ps'
            printPortfolioCashFlow cfs
            return True
        Nothing -> return False

eval (Detail (conds, bond)) = do
    case bond of
        SVar name -> printBnd ("Detail for Bond " ++ name) 
        _ -> printBnd "Detail View"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            det <- getBondDetail b'
            printBondDetail det
            return True
        Nothing -> return False

eval _ = return False

getBondDetail :: MonadBnd m => Bond -> m (Day, Maybe Day, Maybe Day, Maybe Day, Integer, Int, Maybe Money, [Money], [Money], Maybe Money, [Money], Maybe Double)
getBondDetail b = do
    d <- getDate
    p <- getPrice
    let bd = sortByDay $ bondAsList Nothing b
    let after = filterFrom d bd
    let before = filterTo d bd
    let nextDate = if null after then Nothing else Just (tupleDate $ head after)
    let maturityDate = if null after then Nothing else Just (tupleDate $ last after)
    let prevDate = if null before then Nothing else Just (tupleDate $ last before)
    let daysToNext = case nextDate of
                        Nothing -> 0
                        Just nd -> diffDays nd d
    let remainingPayments = length after
    let nominal = getValue bd
    let residual = getValue after
    let accruedInterest = if null after then Nothing else getInterest ((\(_, _, r, _, c, _) -> (r, c)) $ head after) d prevDate nextDate
    let technical = case accruedInterest of
                        Just (ai, c) -> addToAccum (ai, c) residual
                        Nothing -> []
    let parity = case p of
                    Nothing -> Nothing
                    Just (p', c) -> case findMatchingCurrency c technical of
                                    Just (tv, _) -> Just (p' / tv)
                                    Nothing -> Nothing
    return (d, maturityDate, prevDate, nextDate, daysToNext, remainingPayments, p, nominal, residual, accruedInterest, technical, parity)

findMatchingCurrency :: Currency -> [Money] -> Maybe Money
findMatchingCurrency _ [] = Nothing
findMatchingCurrency c ((v, c'):vs) = if c == c' then Just (v, c) else findMatchingCurrency c vs

getInterest :: Money -> Day -> Maybe Day -> Maybe Day -> Maybe Money
getInterest (r,c) d (Just pd) (Just nd) = Just (r * (fromIntegral (diffDays d pd) / fromIntegral (diffDays nd pd)), c)
getInterest _ _ _ _ = Nothing

getValue :: [BondAsTuple] -> [Money]
getValue bonds = sumValue bonds []

sumValue :: [BondAsTuple] -> [Money] -> [Money]
sumValue [] acc = acc
sumValue ((_, _, 0, 0, _, _):bs) acc = sumValue bs acc
sumValue ((_, _, _, a, c, _):bs) acc = sumValue bs (addToAccum (a, c) acc)

addToAccum :: Money -> [Money] -> [Money]
addToAccum (a1, c1) [] = [(a1, c1)]
addToAccum (a1, c1) ((a2, c2):as) = if c1 == c2 then (a1 + a2, c1) : as else (a2, c2) : addToAccum (a1, c1) as

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
applyConds ((CV m):cs) b = do
    setPrice m
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
