module PrettyPrinter where 

import Common
import Sugar
import MonadBnd

import Text.PrettyPrint.Boxes
import Data.Time.Calendar
import Data.Time.Format

printBondCashFlow :: MonadBnd m => Bond -> m ()
printBondCashFlow b = do
    from <- getDate
    let cf = sortByDay $ filterFrom from $ bondAsList b
    let headers = ["Date", "Currency", "Amort", "Rent", "Total", "Scalers"]
    let rowss = map (\(d, a, r, c, s) -> [dateToString d, show c, show a, show r, show (a + r), show s]) cf
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : rowss))

dateToString :: Day -> String
dateToString = formatTime defaultTimeLocale "%d/%m/%Y"
