module PrettyPrinter where

import Common
import Sugar
import MonadBnd

import Data.Time.Calendar (Day)
import Text.PrettyPrint.Boxes

-- generate a box pretty print with the cash flow of a bond
-- generateCashFlowBox :: Bond -> Day -> IO Box
-- generateCashFlowBox b d = do
--   let cf = cashFlow b d
--   let headers = ["Date", "Payment"]
--   let rowss = map (\(d, p) -> [show d, show p]) cf
--   printBox $ hsep 2 left (map (vcat left . map text) (headers : rowss))

-- cashFlow :: Bond -> Day -> [(Day, Double)]
-- cashFlow (At d p) _ = [(d, p)]
-- cashFlow (And b1 b2) d = cashFlow b1 d ++ cashFlow b2 d
-- cashFlow (Scale _ b) d = cashFlow b d

pPrintBond :: MonadBnd m => Bond -> m ()
pPrintBond _ = printBnd $ render (emptyBox 3 3)

