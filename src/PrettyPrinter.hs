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

printBondDetail :: MonadBnd m => (Day, Maybe Day, Maybe Day, Maybe Day, Integer, Maybe (Double, Currency), [(Double, Currency)], [(Double, Currency)], Maybe (Double, Currency), [(Double, Currency)], Maybe Double) -> m ()
printBondDetail (sd, md, lc, nc, dtn, sp, nv, rv, ai, tv, par) = do
    let headers = ["Supposed Date", "Maturity Date", "Last Coupon", "Next Coupon", "Days to Next Coupon", "Supposed Price", "Nominal Value", "Residual Value", "Accrued Interest", "Technical Value", "Parity"]
    let entries = [[dateToString sd, maybeDateToString md, maybeDateToString lc, maybeDateToString nc, show dtn, maybePayToString sp, valueToString nv, valueToString rv, maybePayToString ai, valueToString tv, parityToString par]]
    printBnd $ render $ hsep 2 left (map (vcat left . map text) (headers : entries))

parityToString :: Maybe Double -> String
parityToString Nothing = "N/A"
parityToString (Just p) = show (p * 100) ++ " %"

maybePayToString :: Maybe (Double, Currency) -> String
maybePayToString Nothing = "N/A"
maybePayToString (Just (a, c)) = show a ++ " " ++ show c

valueToString :: [(Double, Currency)] -> String
valueToString [] = "N/A"
valueToString [(a, c)] = show a ++ " " ++ show c
valueToString ((a, c):xs) = show a ++ " " ++ show c ++ ", " ++ valueToString xs
    
maybeDateToString :: Maybe Day -> String
maybeDateToString Nothing = "N/A"
maybeDateToString (Just d) = dateToString d

dateToString :: Day -> String
dateToString = formatTime defaultTimeLocale "%d/%m/%Y"

scalersToString :: [Scaler] -> String
scalersToString [] = "N/A"
scalersToString [x] = show x
scalersToString (x:xs) = show x ++ ", " ++ scalersToString xs

varToString :: Maybe Var -> String
varToString Nothing = "N/A"
varToString (Just v) = v
