{
module Parser where

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
    tyVar       { TkTyVar $$ }
    ':'         { TkTyAscribe }
    '::'        { TkKnAscribe }
    '->'        { TkArrow }
    '*'         { TkKind }
    '['         { TkLSquare }
    ']'         { TkRSquare }
    '('         { TkLParen }
    ')'         { TkRParen }
    ';'         { TkSemi }

%%

Defn :: { [(String, Term)] }
Defn : Def                              { [$1] }
     | Defn ';' Def                     { $3 : $1 }

Def : var '=' Term                      { ($1, $3) }

Term : '\\' var ':' Type '.' Term       { Lam $2 $4 $6 emptyCtx }
     | Term Term                        { App $1 $2 }
     | '\\' tyVar '::' Kind '.' Term    { TyLam $2 $4 $6 emptyCtx }
     | Term '[' Type ']'                { TyApp $1 $3 }
     | '(' Term ')'                     { $2 }
     | var                              { Var $1 }

Type : tyVar                            { TyVar $1 }
     | Type '->' Type                   { TyArr $1 $3 }
     | forall tyVar '::' Kind '.' Type  { Forall $2 $4 $6 }
     | '\\' tyVar '::' Kind '.' Type    { OpLam $2 $4 $6 emptyCtx }
     | Type Type                        { OpApp $1 $2 }
     | '(' Type ')'                     { $2 }


Kind : '*'                              { KnStar }
     | Kind '->' Kind                   { KnArr $1 $3 }
     | '(' Kind ')'                     { $2 }

{
parseTerm :: String -> Term
parseTerm = parse . scan

parseType :: String -> Type
parseType = parseT . scan

parseDefs :: String -> [(String, Term)]
parseDefs = parseD . scan
}
