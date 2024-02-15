module PrettyPrinter (printBondCashFlow, printPortfolioCashFlow) where 

import Text.PrettyPrint.Boxes ( text, hsep, left, render, vcat )
import Data.Time.Calendar ( Day )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Common
import MonadBnd


printBondCashFlow :: MonadBnd m => Bond -> m ()
printBondCashFlow b = do
    from <- getDate
    let headers = ["Date", "Currency", "Amort", "Rent", "Total", "Scalers"]
    let cf = sortByDay $ filterFrom from $ bondAsList b
    let entries = map (\(d, a, r, c, s) -> [dateToString d, show c, show a, show r, show (a + r), scalersToString s]) cf
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

printPortfolioCashFlow :: MonadBnd m => [(Var, Bond)] -> m ()
printPortfolioCashFlow ps = do
    from <- getDate
    let headers = ["Date", "Ticker", "Currency", "Amort", "Rent", "Total", "Scalers"]
    let cfs = sortByDayVar $ filterFromVar from $ concatMap (\(v,b) -> bondAsListWithVar v b) ps
    let entries = map (\(d, v, a, r, c, s) -> [dateToString d, v, show c, show a, show r, show (a + r), scalersToString s]) cfs
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

dateToString :: Day -> String
dateToString = formatTime defaultTimeLocale "%d/%m/%Y"

scalersToString :: [Scaler] -> String
scalersToString [] = ""
scalersToString (x:xs) = show x ++ ", " ++ scalersToString xs
