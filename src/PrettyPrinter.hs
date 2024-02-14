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
    let headers = ["Date", "Amort", "Rent", "Total"]
    let rowss = map (\(d, a, r) -> [dateToString d, moneyToString a, moneyToString r, moneyToString $ addMoney a r]) cf
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : rowss))

moneyToString :: Money -> String
moneyToString (0, _) = "-"
moneyToString (a, c) = show a ++ " " ++ show c

dateToString :: Day -> String
dateToString = formatTime defaultTimeLocale "%d/%m/%Y"
