module Sugar where

import           System.Console.Haskeline

import Data.Time.Calendar (Day, addGregorianMonthsClip)
import Prelude hiding (and, repeat)

import Common

zcb :: Scaler -> Currency -> Day -> Contract
zcb s c d = at d (scale s (one c))

pay :: Scaler -> Currency -> Contract
pay s c = scale s (one c)

repeat :: Int -> Frequency -> Day -> Contract -> Contract
repeat 1 _ d c = at d c
repeat n f d c = at d c `and` repeat (n-1) f (nextDate f d) c

nextDate :: Frequency -> Day -> Day
nextDate Annual d = addGregorianMonthsClip 12 d
nextDate SemiAnnual d = addGregorianMonthsClip 6 d
nextDate Quarterly d = addGregorianMonthsClip 3 d
nextDate Monthly d = addGregorianMonthsClip 1 d


-- argyBond :: Currency -> Day -> Int -> Frequency -> Yield -> Double -> Contract

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
convert _ (SPay s c) = return $ Just (pay s c)
convert env (SRepeat n f d c) = do
    c' <- convert env c
    case c' of
        Just c'' -> return $ Just (repeat n f d c'')
        Nothing -> return $ Nothing

replaceScaler :: Contract -> Scaler -> Double -> Contract
replaceScaler Zero _ _ = Zero
replaceScaler (One c) _ _ = One c
replaceScaler (And c1 c2) s a = And (replaceScaler c1 s a) (replaceScaler c2 s a)
replaceScaler (Or c1 c2) s a = Or (replaceScaler c1 s a) (replaceScaler c2 s a)
replaceScaler (Scale s' c) s a | s' == s = Scale (Mult a) (replaceScaler c s a)
                               | otherwise = Scale s' (replaceScaler c s a)
replaceScaler (At d c) s a = At d (replaceScaler c s a)

-- supose DL 10.0 