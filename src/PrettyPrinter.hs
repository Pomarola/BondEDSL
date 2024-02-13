module PrettyPrinter where 

import Common
import Sugar
import MonadBnd

import Data.Time.Calendar (Day)
import Text.PrettyPrint.Boxes

-- generateCashFlowBox :: Bond -> Day -> IO Box
-- generateCashFlowBox b d = do
--   let cf = cashFlow b d
--   let headers = ["Date", "Amort", "Rent", "Total"]
--   let rowss = map (\(d, a, r) -> [show d, show a, show r, show ((fst a + fst r), snd a)]) cf
--   printBox $ hsep 2 left (map (vcat left . map text) (headers : rowss))

-- cashFlow :: Bond -> Day -> [(Day, Double)]
-- cashFlow (At d p) _ = [(d, p)]
-- cashFlow (And b1 b2) d = cashFlow b1 d ++ cashFlow b2 d
-- cashFlow (Scale _ b) d = cashFlow b d

pPrintBond :: MonadBnd m => Bond -> m ()
pPrintBond b = do
    -- date <- getDate
    -- let cf = cashFlow b date
    let cf = cashFlowNoFrom b
    let headers = ["Date", "Amort", "Rent", "Total"]
    let rowss = map (\(d, a, r) -> [show d, show a, show r, show (fst a + fst r, if snd a == None then snd r else snd a)]) cf
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : rowss))

cashFlow :: Bond -> Day -> [(Day, Money, Money)]
cashFlow (At d PZero) from = if d >= from then [(d, (0, None), (0, None))] else []
cashFlow (At d (Pay a r)) from = if d >= from then [(d, a, r)] else []
cashFlow (And b1 b2) from = cashFlow b1 from ++ cashFlow b2 from
cashFlow (Scale _ b) from = cashFlow b from

cashFlowNoFrom :: Bond -> [(Day, Money, Money)]
cashFlowNoFrom (At d PZero) = [(d, (0, None), (0, None))]
cashFlowNoFrom (At d (Pay a r)) = [(d, a, r)]
cashFlowNoFrom (And b1 b2) = cashFlowNoFrom b1 ++ cashFlowNoFrom b2
cashFlowNoFrom (Scale _ b) = cashFlowNoFrom b
