{
module Parse where
import Sugar
import Bond 
import Data.Maybe
import Data.Char
import Data.Time.Calendar (Day, fromGregorian)
}

%monad { P } { thenP } { returnP }
%name parseDefOrExp DefOrExp
%name parseDefs Defs

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
        '='             { TEquals }
        '('             { TOpen }
        ')'             { TClose }
        '['             { TListOpen }
        ']'             { TListClose }
        '/'             { TSlash }
        ','             { TComma }
        '&'             { TAnd }
        PORTFOLIO       { TPortfolio }
        DEF             { TDef }    
        VAR             { TVar $$ }
        PRINT           { TPrint }
        DATES           { TDates }
        VALUES          { TValues }
        DETAIL          { TDetail }
        CASHFLOW        { TCashflow }
        PORTCASHFLOW    { TPortCashflow }
        SUPPOSE         { TSuppose }
        TODAY           { TToday }
        BCRACER         { TBcracer }
        BCRATC          { TBcratc }
        CURRENT         { TCurrent }
        DATE            { TDate }
        VN              { TVn }
        AT              { TAt }
        SCALE           { TScale }
        RENT            { TRent }
        AMORT           { TAmort }
        ZERO            { TZero }
        CURRENCY        { TCurrency $$ }
        DOUBLE          { TDouble $$ }
        INT             { TInt $$ }
        CER             { TCer }
        DL              { TDl }
        REPEAT          { TRepeat }
        FREQ            { TFreq $$ }
        PERCENT         { TPercent $$ }
        OF              { TOf }
        INTEREST        { TInterest }
    
%right VAR
%left '=' 
%right SCALE
%right '&'

%%

DefOrExp        : Def                                           { $1 }
                | Exp                                           { Eval $1 }

Defs            : Def Defs                                      { $1 : $2 }
                |                                               { [] }
                
Exp             :: {Exp}
                : PRINT SupBond                                 { Print $2 }
                | DATES SupBond                                 { Dates $2 }
                | VALUES SupBond                                { Values $2 }
                | DETAIL SupBond                                { Detail $2 }
                | CASHFLOW SupBond                              { Cashflow $2 }
                | PORTCASHFLOW SupPort                          { PortCashflow $2 }

SupPort         : VAR                                           { ([],$1) }
                | SUPPOSE '[' CondsPort ']' VAR                 { ($3,$5) }

SupBond         :: {CondBond}
                : Bond                                          { ([],$1) }
                | SUPPOSE '[' CondsBond ']' Bond                { ($3,$5) }

Def             : DEF VAR '=' Bond                              { Def $2 $4 } 
                | PORTFOLIO VAR '=' '[' Vars ']'                { Portfolio $2 $5 }

Vars            : INT VAR ',' Vars                              { ($1,$2) : $4 }
                | INT VAR                                       { [($1,$2)] }
                |                                               { [] }

CondsBond       :: {[Cond]}
                : CondBond ',' CondsBond                        { $1 : $3 }
                | CondBond                                      { [$1] }
                |                                               { [] }

CondsPort       :: {[Cond]}
                : CondPort ',' CondsPort                        { $1 : $3 }
                | CondPort                                      { [$1] }
                |                                               { [] }

CondBond        :: {Cond}
                : BCRACER DOUBLE                                { BCCER $2 }
                | BCRATC DOUBLE                                 { BCTC $2 }
                | CURRENT Money                                 { CV $2 }
                | DATE Date                                     { Date $2 }
                | VN INT                                        { VN $2 }
                | TODAY                                         { Today }
                
CondPort        :: {Cond}
                : BCRACER DOUBLE                                { BCCER $2 }
                | BCRATC DOUBLE                                 { BCTC $2 }
                | DATE Date                                     { Date $2 }
                | TODAY                                         { Today }

Bond            :: {SugarBond}
                : Bond '&' Bond                                 { SAnd $1 $3 }
                | '(' Bond ')'                                  { $2 }
                | AT Date Payment                               { SAt $2 $3 }
                | VAR                                           { SVar $1 }
                | SCALE Scaler Bond                             { SScale $2 $3 }
                | Iterate Payment                               { SRepeat $1 $2 }
                | Iterate INTEREST PERCENT OF Money              { SCouponBullet $1 $3 $5 }
                | Iterate INTEREST PERCENT AMORT DOUBLE OF Money { SCouponAmort $1 $3 $5 $7 }
                | Iterate AMORT DOUBLE INTEREST PERCENT OF Money { SCouponAmort $1 $5 $3 $7 }


Iterate         :: {Iterator}
                : REPEAT INT FREQ Date                          { ($2,$3,$4) }

Money           :: {Money}
                : DOUBLE CURRENCY                               { ($1,$2) }

Payment         :: {Payment}
                : RENT DOUBLE AMORT DOUBLE CURRENCY             { Pay $2 $4 $5 }
                | AMORT DOUBLE RENT DOUBLE CURRENCY             { Pay $4 $2 $5 }
                | RENT DOUBLE CURRENCY                          { Pay $2 0 $3 }
                | AMORT DOUBLE CURRENCY                         { Pay 0 $2 $3 }
                | RENT PERCENT OF DOUBLE CURRENCY               { Pay ($2 * $4 / 100) 0 $5 }
                | AMORT PERCENT OF DOUBLE CURRENCY              { Pay 0 ($2 * $4 / 100) $5 }
                | ZERO                                          { PZero }

Scaler          :: {Scaler}
                : DOUBLE                                        { Mult $1 }
                | CER DOUBLE                                    { CER $2 }
                | DL DOUBLE                                     { DolarLinked $2 }
         
Date            :: {Day}
                : INT '/' INT '/' INT                           { fromGregorian (toInteger $5) $3 $1 }

{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TEquals
                | TOpen
                | TClose
                | TListOpen
                | TListClose
                | TSlash
                | TComma
                | TAnd
                | TPortfolio
                | TDef
                | TVar String
                | TPrint
                | TDates
                | TValues
                | TDetail
                | TCashflow
                | TPortCashflow
                | TSuppose
                | TToday
                | TBcracer
                | TBcratc
                | TCurrent
                | TDate
                | TVn
                | TAt
                | TScale
                | TRent
                | TAmort
                | TZero
                | TCurrency String
                | TDouble Double
                | TInt Int
                | TCer
                | TDl
                | TRepeat
                | TFreq Frequency
                | TPercent Double
                | TOf
                | TInterest
                | TEOF
                deriving Show

----------------------------------
lexer cont s = case s of
                [] -> cont TEOF []
                ('\n':s)  ->  \line -> lexer cont s (line + 1)
                (c:cs)
                        | isSpace c -> lexer cont cs
                        | isAlpha c -> lexVar (c:cs)
                        | isDigit c -> lexNum (c:cs)
                ('=':cs) -> cont TEquals cs
                ('(':cs) -> cont TOpen cs
                (')':cs) -> cont TClose cs
                ('[':cs) -> cont TListOpen cs
                (']':cs) -> cont TListClose cs
                ('/':cs) -> cont TSlash cs
                (',':cs) -> cont TComma cs
                ('&':cs) -> cont TAnd cs
                ('$':c:cs)
                        | isAlpha c -> lexCurrency (c:cs)
                        | otherwise -> \line -> Failed $ "Linea "++(show line)++": '$' encontrado pero no se pudo reconocer ninguna moneda"
                unknown -> \line -> Failed $ 
                        "Linea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                where   
                        lexVar cs = case (span isAlphaNum cs) of
                                ("portfolio",rest) -> cont TPortfolio rest
                                ("def",rest) -> cont TDef rest
                                ("print",rest) -> cont TPrint rest
                                ("dates",rest) -> cont TDates rest
                                ("values",rest) -> cont TValues rest
                                ("detail",rest) -> cont TDetail rest
                                ("cashflow",rest) -> cont TCashflow rest
                                ("portcashflow",rest) -> cont TPortCashflow rest
                                ("suppose",rest) -> cont TSuppose rest
                                ("BCTC",rest) -> cont TBcratc rest
                                ("BCCER",rest) -> cont TBcracer rest
                                ("TODAY",rest) -> cont TToday rest
                                ("PRICE",rest) -> cont TCurrent rest
                                ("DATE",rest) -> cont TDate rest
                                ("VN",rest) -> cont TVn rest
                                ("zero",rest) -> cont TZero rest
                                ("at",rest) -> cont TAt rest
                                ("repeat",rest) -> cont TRepeat rest
                                ("rent",rest) -> cont TRent rest
                                ("amort",rest) -> cont TAmort rest
                                ("interest",rest) -> cont TInterest rest
                                ("of",rest) -> cont TOf rest
                                ("scale",rest) -> cont TScale rest
                                ("CER",rest) -> cont TCer rest
                                ("DL",rest) -> cont TDl rest
                                ("ANNUAL",rest) -> cont (TFreq Annual) rest
                                ("SEMIANNUAL",rest) -> cont (TFreq SemiAnnual) rest
                                ("QUARTERLY",rest) -> cont (TFreq Quarterly) rest
                                ("MONTHLY",rest) -> cont (TFreq Monthly) rest
                                (var,rest)   -> cont (TVar var) rest 
                        lexNum cs = let (num,rest) = span isDigit cs 
                                in case rest of
                                        ('.':cs) -> case span isDigit cs of
                                                (num','%':rest') -> cont (TPercent (read (num ++ "." ++ num'))) rest'
                                                (num',rest') -> cont (TDouble (read (num ++ "." ++ num'))) rest'
                                        _ -> cont (TInt (read num)) rest
                        lexCurrency cs = let (curr,rest) = span isAlphaNum cs 
                                in cont (TCurrency curr) rest

defs_parse s = parseDefs s 1
def_or_exp_parse s = parseDefOrExp s 1
}
