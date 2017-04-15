{
module Parser where

import qualified Data.Map.Strict as Map

import Lexer
import Syntax
}

%name parse Term
%name parseT Type
%name parseD Defn
%tokentype { Token }
%error { parseError }

%token
    forall      { TkForall }
    '.'         { TkDot }
    '\\'        { TkLam }
    '='         { TkDef }
    var         { TkVar $$ }
    int         { TkInt $$ }
    tyInt       { TkTyInt }
    tyVar       { TkTyVar $$ }
    static      { TkStatic }
    '&'         { TkBorrow }
    ':'         { TkTyAscribe }
    '::'        { TkKnAscribe }
    '->'        { TkArrow }
    '*'         { TkKind }
    '['         { TkLSquare }
    ']'         { TkRSquare }
    '('         { TkLParen }
    ')'         { TkRParen }
    ';'         { TkSemi }
    ','         { TkComma }
    '<'         { TkLAngle }
    '>'         { TkRAngle }
    '\''        { TkPrime }

%right APP
%%

Defn :: { [(String, Either Type Term)] }
Defn : Def                              { [$1] }
     | Defn ';' Def                     { $3 : $1 }

Def : var '=' Term                      { ($1, Right $3) }
    | tyVar '=' Type                    { ($1, Left $3) }

Litm : '\'' var                         { LiVar $2 }
     | '<' '>'                          { LiDummy }
     | static                           { LiStatic }

Term : '\\' var ':' '(' Litm ',' Type ')' '.' Term       { Lam $2 ($5, $7) $10}
     | Juxt                             { $1 }
     | '&' Term                         { Borrow LiDummy Imm $2 }
     | '\\' tyVar '::' Kind '.' Term    { TyLam $2 $4 $6 Map.empty }
     | '\\''\'' var '.' Term            { LiLam $3 $5 }

Juxt : Juxt Atom                        { App $1 $2 }
     | Term '[' Type ']'                { TyApp $1 $3 }
     | Atom                             { $1 }

Atom : '(' Term ')'                     { $2 }
     | var                              { Var $1 }
     | int                              { Lit $1 }
     | Litm                             { Lt $1 }

Type : tyVar                            { TyVar $1 }
     | tyInt                            { TyInt }
     | Type '->' Type                   { TyArr $1 $3 }
     | forall tyVar '::' Kind '.' Type  { Forall $2 $4 $6 }
     | '\\' tyVar '::' Kind '.' Type    { OpLam $2 $4 $6 Map.empty }
     | Type Type                        { OpApp $1 $2 }
     | '&' Type                         { TyBorrow $2 }
     | '(' Type ')'                     { $2 }

Kind : '*'                              { KnStar }
     | Kind '->' Kind                   { KnArr $1 $3 }
     | '(' Kind ')'                     { $2 }

{
parseTerm :: String -> Term
parseTerm = parse . scan

parseType :: String -> Type
parseType = parseT . scan

parseDefs :: String -> [(String, Either Type Term )]
parseDefs = parseD . scan
}
