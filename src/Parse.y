{
module Parse where
import Common 
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
        DEF             { TDef }    
        VAR             { TVar $$ }
        PRINT           { TPrint }
        TIR             { TTir }
        YIELD           { TYield }
        PRICE           { TPrice }
        SUPPOSE         { TSuppose }
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

    
%right VAR
%left '=' 

%%

DefOrExp        : Def                                           { $1 }
                | Exp                                           { Eval $1 }

Defs            : Def Defs                                      { $1 : $2 }
                |                                               { [] }
                
Exp             :: {Exp}
                : PRINT CondBond                                { Print $2 }
                | TIR CondBond                                  { Tir $2 }
                | YIELD CondBond                                { Yield $2 }
                | PRICE CondBond                                { Price $2 }

CondBond        :: {CondBond}
                : Bond                                          { ([],$1) }
                | SUPPOSE '[' Conds ']' Bond                    { ($3,$5) }

Def             : DEF VAR '=' Bond                              { Def $2 $4 }       

Conds           : Cond ',' Conds                                { $1 : $3 }
                | Cond                                          { [$1] }
                |                                               { [] }

Cond            :: {Cond}
                : BCRACER DOUBLE                                { BCCER $2 }
                | BCRATC DOUBLE                                 { BCTC $2 }
                | CURRENT DOUBLE                                { CV $2 }
                | DATE Date                                     { Date $2 }
                | VN INT                                        { VN $2 }

Bond            :: {SugarBond}
                : Bond '&' Bond                                 { SAnd $1 $3 }
                | AT Date Payment                               { SAt $2 $3 }
                | VAR                                           { SVar $1 }
                | SCALE Scaler Bond                             { SScale $2 $3 }
                | REPEAT INT FREQ Date Payment                  { SRepeat $2 $3 $4 $5 }

Payment         :: {Payment}
                : RENT DOUBLE CURRENCY AMORT DOUBLE CURRENCY    { Pay ($2,$3) ($5,$6) }
                | AMORT DOUBLE CURRENCY RENT DOUBLE CURRENCY    { Pay ($5,$6) ($2,$3) }
                | RENT DOUBLE CURRENCY                          { Pay ($2,$3) (0, None) }
                | AMORT DOUBLE CURRENCY                         { Pay (0, None) ($2,$3) }
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
                | TDef
                | TVar String
                | TPrint
                | TTir
                | TYield
                | TPrice
                | TSuppose
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
                | TCurrency Currency
                | TDouble Double
                | TInt Int
                | TCer
                | TDl
                | TRepeat
                | TFreq Frequency
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
                unknown -> \line -> Failed $ 
                        "Linea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                where   
                        lexVar cs = case (span isAlpha cs) of
                                ("def",rest) -> cont TDef rest
                                ("repeat",rest) -> cont TRepeat rest
                                ("print",rest) -> cont TPrint rest
                                ("tir",rest) -> cont TTir rest
                                ("yield",rest) -> cont TYield rest
                                ("price",rest) -> cont TPrice rest
                                ("suppose",rest) -> cont TSuppose rest
                                ("BCCER",rest) -> cont TBcracer rest
                                ("BCTC",rest) -> cont TBcratc rest
                                ("PRICE",rest) -> cont TCurrent rest
                                ("DATE",rest) -> cont TDate rest
                                ("VN",rest) -> cont TVn rest
                                ("at",rest) -> cont TAt rest
                                ("scale",rest) -> cont TScale rest
                                ("rent",rest) -> cont TRent rest
                                ("amort",rest) -> cont TAmort rest
                                ("zero",rest) -> cont TZero rest
                                ("CER",rest) -> cont TCer rest
                                ("DL",rest) -> cont TDl rest
                                ("USD",rest) -> cont (TCurrency USD) rest
                                ("ARS",rest) -> cont (TCurrency ARS) rest
                                ("BTC",rest) -> cont (TCurrency BTC) rest
                                ("ETH",rest) -> cont (TCurrency ETH) rest
                                ("ANNUAL",rest) -> cont (TFreq Annual) rest
                                ("SEMIANNUAL",rest) -> cont (TFreq SemiAnnual) rest
                                ("QUARTERLY",rest) -> cont (TFreq Quarterly) rest
                                ("MONTHLY",rest) -> cont (TFreq Monthly) rest
                                (var,rest)   -> cont (TVar var) rest 
                        lexNum cs = let (num,rest) = span isDigit cs 
                                in case rest of
                                        ('.':cs) -> let (num',rest') = span isDigit cs
                                                in cont (TDouble (read (num ++ "." ++ num'))) rest'
                                        _ -> cont (TInt (read num)) rest
                                           
defs_parse s = parseDefs s 1
def_or_exp_parse s = parseDefOrExp s 1
}
