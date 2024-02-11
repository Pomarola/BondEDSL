module Sugar where

import           System.Console.Haskeline

import Data.Time.Calendar (Day, addGregorianMonthsClip)
import Prelude hiding (and, repeat)

import Common
import Env
import MonadBnd

repeat :: Int -> Frequency -> Day -> Payment -> Bond
repeat 1 _ d p = at d p
repeat n f d p = at d p `and` repeat (n-1) f (nextDate f d) p

nextDate :: Frequency -> Day -> Day
nextDate Annual d = addGregorianMonthsClip 12 d
nextDate SemiAnnual d = addGregorianMonthsClip 6 d
nextDate Quarterly d = addGregorianMonthsClip 3 d
nextDate Monthly d = addGregorianMonthsClip 1 d

convert :: MonadBnd m => SugarBond -> m (Maybe Bond)
convert _ = return Nothing


-- convert :: Env -> SugarBond -> InputT IO (Maybe Bond)
-- convert env (SVar v) = case findBond env v of
--     Just b -> return $ Just b
--     Nothing -> return $ Nothing
-- convert env (SAnd b1 b2) = do
--     b1' <- convert env b1
--     case b1' of
--         Just b1'' -> do
--             b2' <- convert env b2
--             case b2' of
--                 Just b2'' -> return $ Just (And b1'' b2'')
--                 Nothing -> return $ Nothing
--         Nothing -> return $ Nothing
-- convert env (SScale s b) = do
--     b' <- convert env b
--     case b' of
--         Just b'' -> return $ Just (Scale s b'')
--         Nothing -> return $ Nothing
-- convert _ (SAt d p) = return $ Just (At d p)
-- convert _ (SRepeat n f d p) = return $ Just (repeat n f d p)
-- -- convert _ (SZcb s c d) = return $ Just (zcb s c d)
-- -- convert _ (SPay s c) = return $ Just (pay s c)
-- -- convert env (SRepeat n f d c) = do
--     -- c' <- convert env c
--     -- case c' of
--     --     Just c'' -> return $ Just (repeat n f d c'')
--     --     Nothing -> return $ Nothing

-- -- supose DL 10.0 