module PrettyPrinter (printBondCashFlow, printPortfolioCashFlow, printBondDetail, printBondDates, printBondValues) where

import Text.PrettyPrint.Boxes ( text, hsep, left, render, vcat )
import Data.Time.Calendar ( Day )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Text.Printf ( printf )
import Bond
import MonadBnd


printBondCashFlow :: MonadBnd m => [BondAsTuple] -> m ()
printBondCashFlow cf = do
    let headers = ["Date", "Currency", "Rent", "Amort", "Total", "Scalers"]
    let entries = map (\(d, _, r, a, c, s) -> [dateToString d, c, showDouble r, showDouble a, showDouble (a + r), scalersToString s]) cf
    printBnd $ verticalTable (headers : entries)

printPortfolioCashFlow :: MonadBnd m => [BondAsTuple] -> m ()
printPortfolioCashFlow cfs = do
    let headers = ["Date", "Ticker", "Currency", "Rent", "Amort", "Total", "Scalers"]
    let entries = map (\(d, v, r, a, c, s) -> [dateToString d, varToString v, c, showDouble r, showDouble a, showDouble (a + r), scalersToString s]) cfs
    printBnd $ verticalTable (headers : entries)

printBondDetail :: MonadBnd m => (Day, Maybe Day, Maybe Day, Maybe Day, Int, Int) -> (Maybe Money, [Money], [Money], Maybe Money, [Money], Maybe Double) -> m ()
printBondDetail d v = let (h1, e1) = generateDatesTable d
                          (h2, e2) = generateValsTable v
                      in printBnd $ horizontalTable $ (h1 : e1) ++ (h2 : e2)

printBondDates :: MonadBnd m => (Day, Maybe Day, Maybe Day, Maybe Day, Int, Int) -> m ()
printBondDates b = let (h, e) = generateDatesTable b
                    in printBnd $ horizontalTable (h : e)

printBondValues :: MonadBnd m => (Maybe Money, [Money], [Money], Maybe Money, [Money], Maybe Double) -> m ()
printBondValues b = let (h, e) = generateValsTable b
                    in printBnd $ horizontalTable (h : e)

generateValsTable :: (Maybe Money, [Money], [Money], Maybe Money, [Money], Maybe Double) -> ([String], [[String]])
generateValsTable (sp, nv, rv, ai, tv, par) = (headers, entries)
    where headers = ["Supposed Price", "Nominal Value", "Residual Value", "Accrued Interest", "Technical Value", "Parity"]
          entries = [[maybePayToString sp, valueToString nv, valueToString rv, maybePayToString ai, valueToString tv, parityToString par]]

generateDatesTable :: (Day, Maybe Day, Maybe Day, Maybe Day, Int, Int) -> ([String], [[String]])
generateDatesTable (sd, md, lc, nc, dtn, remp) = (headers, entries)
    where headers = ["Supposed Date", "Maturity Date", "Last Coupon", "Next Coupon", "Days to Next Coupon", "Remaining Payments"]
          entries = [[dateToString sd, maybeDateToString md, maybeDateToString lc, maybeDateToString nc, show dtn, show remp]]

horizontalTable :: [[String]] -> String
horizontalTable list = render $ hsep 5 left (map (vcat left . map text) list)

verticalTable :: [[String]] -> String
verticalTable list = render $ vcat left (map (hsep 10 left . map (text . align)) list)
    where align s = replicate (10 - length s) ' ' ++ s

parityToString :: Maybe Double -> String
parityToString Nothing = "N/A"
parityToString (Just p) = showDouble (p * 100) ++ " %"

maybePayToString :: Maybe Money -> String
maybePayToString Nothing = "N/A"
maybePayToString (Just (a, c)) = showDouble a ++ " " ++ c

valueToString :: [Money] -> String
valueToString [] = "N/A"
valueToString [(a, c)] = showDouble a ++ " " ++ c
valueToString ((a, c):xs) = showDouble a ++ " " ++ c ++ ", " ++ valueToString xs

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

showDouble :: Double -> String
showDouble = printf "%.3f"