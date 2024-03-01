module Eval (eval) where

import Control.Monad.Cont (MonadIO(liftIO))
import Data.Time.Calendar (Day, diffDays)
import Bond
import Sugar
import MonadBnd
import PrettyPrinter
import State

eval :: MonadBnd m => Exp -> m Bool

-- Evalua un bono y muestra el flujo de fondos
eval (Cashflow conds bond) = do
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

-- Evalua un portafolio y muestra el flujo de fondos de cada bono que lo compone
eval (PortCashflow conds var) = do
    printBnd $ "Cashflow for portfolio " ++ var
    p <- lookupPortfolio var
    case p of
        Just ps -> do
            ps' <- evalPort conds ps
            case ps' of
                [] -> return False
                _ -> do
                        from <- getDate
                        let cfs = sortByDay $ filterFrom from $ concatMap (\(v,b) -> bondAsList (Just v) b) ps'
                        printPortfolioCashFlow cfs
                        return True
        Nothing -> return False

-- Evalua un bono y muestra el detalle completo (fechas y valores relevantes)
eval (Detail conds bond) = do
    case bond of
        SVar name -> printBnd ("Full Detail for Bond " ++ name) 
        _ -> printBnd "Full Detail View"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            dates <- getBondDates $ sortedBond b'
            vals <- getBondValues $ sortedBond b'
            printBondDetail dates vals
            return True
        Nothing -> return False

-- Evalua un bono y muestra las fechas relevantes
eval (Dates conds bond) = do
    case bond of
        SVar name -> printBnd ("Dates for Bond " ++ name) 
        _ -> printBnd "Dates View"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            dates <- getBondDates $ sortedBond b'
            printBondDates dates
            return True
        Nothing -> return False
    
-- Evalua un bono y muestra los valores teoricos
eval (Values conds bond) = do
    case bond of
        SVar name -> printBnd ("Values for Bond " ++ name) 
        _ -> printBnd "Values View"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            vals <- getBondValues $ sortedBond b'
            printBondValues vals
            return True
        Nothing -> return False

-- Evalua un bono y muestra las tuplas que lo conforman
eval (Print conds bond) = do
    case bond of
        SVar name -> printBnd ("Tuples for Bond " ++ name) 
        _ -> printBnd "Tuple View"
    b <- convertCond conds bond
    case b of
        Just b' -> do
            from <- getDate
            printTuples $ sortByDay $ filterFrom from $ bondAsList Nothing b'
            return True
        Nothing -> return False

sortedBond :: Bond -> [BondAsTuple]
sortedBond b = sortByDay $ bondAsList Nothing b

-- Obtiene las fechas relevantes de un bono
getBondDates :: MonadBnd m => [BondAsTuple] -> m (Day, Maybe Day, Maybe Day, Maybe Day, Int, Int)
getBondDates b = do
    d <- getDate
    let after = filterFrom d b
    let before = filterTo d b
    let nextDate = if null after then Nothing else Just (tupleDate $ head after)
    let maturityDate = if null after then Nothing else Just (tupleDate $ last after)
    let prevDate = if null before then Nothing else Just (tupleDate $ last before)
    let daysToNext = case nextDate of
                        Nothing -> 0
                        Just nd -> fromInteger $ diffDays nd d
    let remainingPayments = length after
    return (d, maturityDate, prevDate, nextDate, daysToNext, remainingPayments)

-- Obtiene los valores relevantes de un bono
getBondValues :: MonadBnd m => [BondAsTuple] -> m (Maybe Money, [Money], [Money], Maybe Money, [Money], Maybe Double)
getBondValues b = do
    p <- getPrice
    d <- getDate
    let after = filterFrom d b
    let nominal = getValue b
    let residual = getValue after
    let before = filterTo d b
    let nextDate = if null after then Nothing else Just (tupleDate $ head after)
    let prevDate = if null before then Nothing else Just (tupleDate $ last before)
    let accruedInterest = if null after then Nothing else getInterest ((\(_, _, r, _, c, _) -> (r, c)) $ head after) d prevDate nextDate
    let technical = case accruedInterest of
                        Just (ai, c) -> addToAccum (ai, c) residual
                        Nothing -> []
    let parity = case p of
                    Nothing -> Nothing
                    Just (p', c) -> case findMatchingCurrency c technical of
                                    Just (tv, _) -> Just (p' / tv)
                                    Nothing -> Nothing
    return (p, nominal, residual, accruedInterest, technical, parity)

findMatchingCurrency :: Currency -> [Money] -> Maybe Money
findMatchingCurrency _ [] = Nothing
findMatchingCurrency c ((v, c'):vs) = if c == c' then Just (v, c) else findMatchingCurrency c vs

-- Obtiene el interes acumulado entre dos fechas (intereses corridos)
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

-- Evalua un portafolio aplicando las condiciones supuestas y la cantidad a cada bono que lo compone
evalPort :: MonadBnd m => [Cond] -> [(Int, Var)] -> m [(Var, Bond)]
evalPort _ [] = return []
evalPort conds ((n, v):ps) = do
    b <- lookupDef v
    case b of
        Just b' -> do
            bs <- evalPort conds ps
            bc <- applyConds conds b'
            return ((v, applyScalers (fromIntegral n) bc) : bs)
        Nothing -> do
            printBnd ("No se encontro " ++ v ++ " en el entorno.")
            return []

-- Convierte un SugarBond a un Bond y aplica las condiciones supuestas
convertCond :: MonadBnd m => [Cond] -> SugarBond -> m (Maybe Bond)
convertCond conds sb = do
    b <- convert sb
    case b of
        Just b' -> do
            cb <- applyConds conds b'
            return $ Just (evalScalers cb)
        Nothing -> return Nothing

-- Aplica las condiciones a un bono
applyConds :: MonadBnd m => [Cond] -> Bond -> m Bond
applyConds [] b = return b
applyConds ((BCCER cer):cs) b = let b' = replaceScaler (CER cer) b in applyConds cs b'
applyConds ((BCUSD tc):cs) b = let b' = replaceScaler (DolarLinked tc) b in applyConds cs b'
applyConds ((Quantity n):cs) b = applyConds cs (Scale (Mult (fromIntegral n)) b)
applyConds ((Market m):cs) b = do
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

-- Aplica los escaladores que sean simples multiplicadores a un bono
applyScalers :: Double -> Bond -> Bond
applyScalers m (Scale (Mult s) b) = applyScalers (m * s) b
applyScalers m (Scale s@(CER _) b) = Scale s (applyScalers m b)
applyScalers m (Scale s@(DolarLinked _) b) = Scale s (applyScalers m b)
applyScalers m (And b1 b2) = And (applyScalers m b1) (applyScalers m b2)
applyScalers _ b@(At _ PZero) = b
applyScalers m (At d (Pay r a c)) = At d (Pay (r * m) (a * m) c)

-- Utilizado para aplicar los condicionadores supuestos a los escaladores de un bono que esten ligados a una medida
replaceScaler :: Scaler -> Bond -> Bond
replaceScaler n@(CER new) (Scale (CER old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n@(DolarLinked new) (Scale (DolarLinked old) b) = Scale (Mult (new / old)) (replaceScaler n b)
replaceScaler n (Scale m b) = Scale m (replaceScaler n b)
replaceScaler n (And b1 b2) = And (replaceScaler n b1) (replaceScaler n b2)
replaceScaler _ b@(At _ _) = b
