{
module Lexer
    ( Token (..)
    , scan
    ) where
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]

tokens :-
    $white+             ;
    "--".*              ;
    "Int"               { \s -> TkTyInt }
    forall              { \s -> TkForall }
    static              { \s -> TkStatic }
    "0"                 { \s -> TkInt (read s) }
    [1-9][$digit]*      { \s -> TkInt (read s) }
    \.                  { \s -> TkDot }
    \.                  { \s -> TkDot }
    \\                  { \s -> TkLam }
    \=                  { \s -> TkDef }
    $lower [$alpha]*    { \s -> TkVar s }
    $upper [$alpha]*    { \s -> TkTyVar s }
    "->"                { \s -> TkArrow }
    "::"                { \s -> TkKnAscribe }
    \:                  { \s -> TkTyAscribe }
    \&                  { \s -> TkBorrow }
    \*                  { \s -> TkKind }
    \[                  { \s -> TkLSquare }
    \]                  { \s -> TkRSquare }
    \(                  { \s -> TkLParen }
    \)                  { \s -> TkRParen }
    \;                  { \s -> TkSemi }
    \'                  { \s -> TkPrime }
    \<                  { \s -> TkLAngle }
    \>                  { \s -> TkRAngle }
    \,                  { \s -> TkComma }

{

data Token
    = TkDot
    | TkLam
    | TkDef
    | TkVar String
    | TkTyVar String
    | TkInt Int
    | TkTyInt
    | TkTyAscribe
    | TkKnAscribe
    | TkArrow
    | TkKind
    | TkLSquare
    | TkRSquare
    | TkForall
    | TkLParen
    | TkRParen
    | TkSemi 
    | TkLAngle
    | TkRAngle
    | TkPrime
    | TkComma
    | TkStatic
    | TkBorrow
    deriving (Eq, Show)

scan = alexScanTokens

}
