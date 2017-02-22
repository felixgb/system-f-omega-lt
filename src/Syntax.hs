{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax where

import Control.Monad.Except

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Ctx = Map.Map String

ctxLookup :: Bindable a => String -> Ctx a -> ThrowsError a
ctxLookup var ctx = case Map.lookup var ctx of
    (Just val) -> return val
    Nothing -> throwError $ VarNotFound var

ctxInsert :: Bindable a => String -> a -> Ctx a -> Ctx a
ctxInsert = Map.insert

data Term
    = Var String
    | Lit Int
    | Lam String Type Term (Ctx Term)
    | App Term Term
    | TyLam String Kind Term (Ctx Type)
    | TyApp Term Type
    | Brack Term
    deriving (Eq, Show)

data Type
    = TyVar String
    | TyInt
    | TyArr Type Type
    | Forall String Kind Type
    | OpLam String Kind Type (Ctx Type)
    | OpApp Type Type
    deriving (Show)

instance Eq Type where
    t1@(OpLam var kn body clos) == t2 = t1 == apply newSubst body
        where newSubst = Subst $ Map.singleton var t2
    (TyVar v1) == (TyVar v2) = v1 == v2
    (TyArr t11 t12) == (TyArr t21 t22) = t11 == t21 && t12 == t22
    (Forall v1 k1 t1) == (Forall v2 k2 t2) = v1 == v2 && k1 == k2 && t1 == t2
    (OpApp t11 t12) == (OpApp t21 t22) = t11 == t21 && t12 == t22
    TyInt == TyInt = True
    _ == _ = False

data Kind
    = KnStar
    | KnArr Kind Kind
    deriving (Eq, Show)

emptyCtx :: Map.Map String a
emptyCtx = Map.empty

type ThrowsError = Except LangErr

data LangErr
    = ParseError
    | NoMain
    | VarNotFound String
    | WrongKind Kind Kind
    | WrongType Type Type
    deriving (Eq, Show)

parseError x = error $ show x

-- Subst stuff

newtype Subst a = Subst (Map.Map String a)
    deriving (Eq, Ord, Show, Monoid)

class Substable a where
    apply :: Subst a -> a -> a
    ftv :: a -> Set.Set String

instance Substable Type where
    apply (Subst s) var@(TyVar x) = Map.findWithDefault var x s
    apply s (TyArr t1 t2) = apply s t1 `TyArr` apply s t2
    apply s (Forall var kn ty) = Forall var kn (apply s ty)
    apply s (OpLam var kn ty ctx) = OpLam var kn (apply s ty) ctx
    apply s (OpApp t1 t2) = OpApp (apply s t1) (apply s t2)

    ftv (TyVar name) = Set.singleton name
    ftv (TyArr t1 t2) = ftv t1 `Set.union` ftv t2
    ftv (Forall var kn ty) = ftv ty
    ftv (OpLam var kn ty clos) = ftv ty `Set.difference` Set.singleton var
    ftv (OpApp t1 t2) = ftv t1 `Set.union` ftv t2

compose :: Substable a => Subst a -> Subst a -> Subst a
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s1

class Bindable a where

instance Bindable Kind where
instance Bindable Type where
instance Bindable Term where

