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
    forall              { \s -> TkForall }
    \.                  { \s -> TkDot }
    \\                  { \s -> TkLam }
    \=                  { \s -> TkDef }
    $lower [$alpha]*    { \s -> TkVar s }
    $upper [$alpha]*    { \s -> TkTyVar s }
    "->"                { \s -> TkArrow }
    "::"                { \s -> TkKnAscribe }
    \:                  { \s -> TkTyAscribe }
    \*                  { \s -> TkKind }
    \[                  { \s -> TkLSquare }
    \]                  { \s -> TkRSquare }
    \(                  { \s -> TkLParen }
    \)                  { \s -> TkRParen }
    \;                  { \s -> TkSemi }

{

data Token
    = TkDot
    | TkLam
    | TkDef
    | TkVar String
    | TkTyVar String
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
    deriving (Eq, Show)

scan = alexScanTokens

}
