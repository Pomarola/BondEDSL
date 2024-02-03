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
      '='     { TEquals }
      '('     { TOpen }
      ')'     { TClose }
      '/'     { TBar }
      DEF     { TDef }    
      ZERO    { TZero }
      VAR     { TVar $$ }
      AND     { TAnd }
      OR      { TOr }
      ONE     { TOne }
      AT      { TAt }
      SCALE   { TScale }
      DOUBLE  { TDouble $$ }
      CER     { TCer }
      DL      { TDL }
      INT     { TInt $$ }
      CURRENCY { TCurrency $$ }
      PRINT    { TPrint }
      TIR      { TTir }
      YIELD    { TYield }
      PRICE    { TPrice }
      ZCB      { TZcb }
      REPEAT   { TRepeat }
      PAY      { TPay }
      FREQ     { TFreq $$ }
      SUPPOSE  { TSuppose }
      BCRACER  { TBcraCer }
      BCRATC   { TBcraTc }
      CURRENT  { TCurrent }
      DATE     { TDate }
    

%right VAR
%left '=' 
%right AND
%right OR

%%

DefOrExp        : Def                                           { $1 }
                | Exp                                           { Eval $1 }

Defs            : Def Defs                                      { $1 : $2 }
                |                                               { [] }
                
Exp             :: {Exp}
                : PRINT CondBond                            { Print $2 }
                | TIR CondBond                              { Tir $2 }
                | YIELD CondBond                            { Yield $2 }
                | PRICE CondBond                            { Price $2 }

CondBond    :: {CondBond}
                : Bond                                      { SC $1 }
                | SUPPOSE Cond Bond                         { CC $3 $2 }

Def             : DEF VAR '=' Bond                          { Def $2 $4 }       

Bond        :: {SugarBond}
                : AT Date ClosedBond                        { SAt $2 $3 }
                | Bond AND Bond                         { SAnd $1 $3 }
                | Bond OR Bond                          { SOr $1 $3 }
                | ZCB Scaler CURRENCY Date                      { SZcb $2 $3 $4 }
                | REPEAT INT FREQ Date Bond                 { SRepeat $2 $3 $4 $5 }
                | VAR                                           { SVar $1 }

ClosedBond  :: {SugarBond} 
                : ZERO                                          { SZero }
                | ONE CURRENCY                                  { SOne $2 }
                | SCALE Scaler ClosedBond                   { SScale $2 $3 }
                | ClosedBond AND ClosedBond             { SAnd $1 $3 }
                | ClosedBond OR ClosedBond              { SOr $1 $3 }
                | '(' ClosedBond ')'                        { $2 }
                | PAY Scaler CURRENCY                           { SPay $2 $3}

Cond            :: {Cond}
                : BCRACER DOUBLE                                { BCCER $2 }
                | BCRATC DOUBLE                                 { BCTC $2 }
                | CURRENT DOUBLE                                { CV $2 }
                | DATE Date                                     { Date $2 }

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

data Token = TVar String
                | TEquals
                | TOpen
                | TClose
                | TBar
                | TDef
                | TZero
                | TAnd
                | TOr
                | TOne
                | TAt
                | TScale
                | TDouble Double
                | TCer
                | TDL
                | TInt Int
                | TCurrency Currency
                | TPrint
                | TTir
                | TYield
                | TPrice
                | TZcb
                | TPay
                | TRepeat
                | TFreq Frequency
                | TSuppose
                | TBcraCer
                | TBcraTc
                | TCurrent
                | TDate
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
                ('/':cs) -> cont TBar cs
                unknown -> \line -> Failed $ 
                        "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                where   
                        lexVar cs = case (span isAlpha cs) of
                                ("def",rest) -> cont TDef rest
                                ("zero",rest) -> cont TZero rest
                                ("one",rest) -> cont TOne rest
                                ("at",rest) -> cont TAt rest
                                ("scale",rest) -> cont TScale rest
                                ("and",rest) -> cont TAnd rest
                                ("or",rest) -> cont TOr rest
                                ("print",rest) -> cont TPrint rest
                                ("tir",rest) -> cont TTir rest
                                ("yield",rest) -> cont TYield rest
                                ("price",rest) -> cont TPrice rest
                                ("zcb",rest) -> cont TZcb rest
                                ("pay",rest) -> cont TPay rest
                                ("repeat",rest) -> cont TRepeat rest
                                ("suppose",rest) -> cont TSuppose rest
                                ("CER",rest) -> cont TCer rest
                                ("DL",rest) -> cont TDL rest
                                ("USD",rest) -> cont (TCurrency USD) rest
                                ("ARS",rest) -> cont (TCurrency ARS) rest
                                ("BTC",rest) -> cont (TCurrency BTC) rest
                                ("ETH",rest) -> cont (TCurrency ETH) rest
                                ("ANNUAL",rest) -> cont (TFreq Annual) rest
                                ("SEMIANNUAL",rest) -> cont (TFreq SemiAnnual) rest
                                ("QUARTERLY",rest) -> cont (TFreq Quarterly) rest
                                ("MONTHLY",rest) -> cont (TFreq Monthly) rest
                                ("BCRA-TC",rest) -> cont TBcraTc rest
                                ("BCRA-CER",rest) -> cont TBcraCer rest
                                ("CURRENT",rest) -> cont TCurrent rest
                                ("DATE",rest) -> cont TDate rest
                                (var,rest)   -> cont (TVar var) rest 
                        lexNum cs = let (num,rest) = span isDigit cs 
                                in case rest of
                                        ('.':cs) -> let (num',rest') = span isDigit cs
                                                in cont (TDouble (read (num ++ "." ++ num'))) rest'
                                        _ -> cont (TInt (read num)) rest
                                           
defs_parse s = parseDefs s 1
def_or_exp_parse s = parseDefOrExp s 1
}
