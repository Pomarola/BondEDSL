module Sugar where

import           System.Console.Haskeline

import Common

zcb :: Scaler -> Currency -> Date -> Contract
zcb s c d = at d (scale s (one c))

-- argyBond :: Currency -> Date -> Int -> Frequency -> Yield -> Double -> Contract

-- loopZcb :: 
-- loopZcb 

-- many para hacer que dado una cantidad haga todos los ands. aunque no se si tiene mucho sentido por las fechas. por ahi darle una periodicidad a fechas.

findContract :: Env -> Var -> Maybe Contract
findContract [] _ = Nothing
findContract ((v,c):xs) v' = if v == v' then Just c else findContract xs v'

convert :: Env -> SugarContract -> InputT IO (Maybe Contract)
convert env (SVar v) = case findContract env v of
    Just c -> return $ Just c
    Nothing -> return $ Nothing
convert _ SZero = return $ Just Zero
convert _ (SOne c) = return $ Just (One c)
convert env (SAnd c1 c2) = do
    c1' <- convert env c1
    case c1' of
        Just c1'' -> do
            c2' <- convert env c2
            case c2' of
                Just c2'' -> return $ Just (And c1'' c2'')
                Nothing -> return $ Nothing
        Nothing -> return $ Nothing
convert env (SOr c1 c2) = do
    c1' <- convert env c1
    case c1' of
        Just c1'' -> do
            c2' <- convert env c2
            case c2' of
                Just c2'' -> return $ Just (Or c1'' c2'')
                Nothing -> return $ Nothing
        Nothing -> return $ Nothing
convert env (SGive c) = do
    c' <- convert env c
    case c' of
        Just c'' -> return $ Just (Give c'')
        Nothing -> return $ Nothing
convert env (SScale s c) = do
    c' <- convert env c
    case c' of
        Just c'' -> return $ Just (Scale s c'')
        Nothing -> return $ Nothing
convert env (SAt d c) = do 
    c' <- convert env c
    case c' of
        Just c'' -> return $ Just (At d c'')
        Nothing -> return $ Nothing
convert _ (SZcb s c d) = return $ Just (zcb s c d)