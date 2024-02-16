module PrettyPrinter (printBondCashFlow, printPortfolioCashFlow, printBondDetail) where 

import Text.PrettyPrint.Boxes ( text, hsep, left, render, vcat )
import Data.Time.Calendar ( Day )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Common
import MonadBnd


printBondCashFlow :: MonadBnd m => [BondAsTuple] -> m ()
printBondCashFlow cf = do
    let headers = ["Date", "Currency", "Amort", "Rent", "Total", "Scalers"]
    let entries = map (\(d, _, a, r, c, s) -> [dateToString d, show c, show a, show r, show (a + r), scalersToString s]) cf
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

printPortfolioCashFlow :: MonadBnd m => [BondAsTuple] -> m ()
printPortfolioCashFlow cfs = do
    let headers = ["Date", "Ticker", "Currency", "Amort", "Rent", "Total", "Scalers"]
    let entries = map (\(d, v, a, r, c, s) -> [dateToString d, varToString v, show c, show a, show r, show (a + r), scalersToString s]) cfs
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

printBondDetail :: MonadBnd m => (Day, Maybe Day, Maybe Day, Maybe Day, Integer, [(Double, Currency)]) -> m ()
printBondDetail (sd, md, lc, nc, dtn, nv) = do
    let headers = ["Supposed Date", "Maturity Date", "Last Coupon", "Next Coupon", "Days to Next Coupon", "Nominal Value"]
    let entries = [[dateToString sd, maybeDateToString md, maybeDateToString lc, maybeDateToString nc, show dtn, amortToString nv]]
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

amortToString :: [(Double, Currency)] -> String
amortToString [] = ""
amortToString [(a, c)] = show a ++ " " ++ show c
amortToString ((a, c):xs) = show a ++ " " ++ show c ++ ", " ++ amortToString xs
    
maybeDateToString :: Maybe Day -> String
maybeDateToString Nothing = "-"
maybeDateToString (Just d) = dateToString d

dateToString :: Day -> String
dateToString = formatTime defaultTimeLocale "%d/%m/%Y"

scalersToString :: [Scaler] -> String
scalersToString [] = ""
scalersToString [x] = show x
scalersToString (x:xs) = show x ++ ", " ++ scalersToString xs

varToString :: Maybe Var -> String
varToString Nothing = "-"
varToString (Just v) = v
