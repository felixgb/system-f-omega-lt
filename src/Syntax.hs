{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Subst a = Subst (Map.Map String a)
    deriving (Eq, Ord, Show, Monoid)

class Substable a where
    apply :: Subst a -> a -> a
    ftv :: a -> Set.Set String

compose :: Substable a => Subst a -> Subst a -> Subst a
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply $ Subst s1) s2 `Map.union` s1

-- All contexts are a mapping of var names to values
type Ctx = Map.Map String

class Bindable a where

instance Bindable Kind where
instance Bindable Type where
instance Bindable Term where
instance Bindable Lifetime where

data Lifetime 
    = LiVar String
    | LiLit Int
    | LiStatic
    | LiDummy
    deriving (Eq, Show)

data Term
    = Var String
    | Lit Int
    | Lam String (Lifetime, Type) Term
    | App Term Term
    | TyLam String Kind Term (Ctx Type)
    | TyApp Term Type
    | LiLam String Term
    | LiApp Term Term
    | Lt Lifetime
    deriving (Eq, Show)

data Type
    = TyVar String
    | TyInt
    | TyArr Type Type
    | Forall String Kind Type
    | OpLam String Kind Type (Ctx Type)
    | OpApp Type Type
    deriving (Eq, Show)

instance Substable Type where
    apply _ TyInt = TyInt
    apply (Subst s) var@(TyVar x) = Map.findWithDefault var x s
    apply s (TyArr t1 t2) = apply s t1 `TyArr` apply s t2
    apply s (Forall var kn ty) = Forall var kn (apply s ty)
    apply s (OpLam var kn ty ctx) = OpLam var kn (apply s ty) ctx
    apply s (OpApp t1 t2) = OpApp (apply s t1) (apply s t2)

    ftv TyInt = Set.empty
    ftv (TyVar name) = Set.singleton name
    ftv (TyArr t1 t2) = ftv t1 `Set.union` ftv t2
    ftv (Forall var kn ty) = ftv ty
    ftv (OpLam var kn ty clos) = ftv ty `Set.difference` Set.singleton var
    ftv (OpApp t1 t2) = ftv t1 `Set.union` ftv t2

data Kind
    = KnStar
    | KnArr Kind Kind
    deriving (Eq, Show)

data Env = Env
    { _typeCtx :: Ctx Type
    , _kindCtx :: Ctx Kind
    } deriving (Show, Eq)

data LangErr
    = ParseError
    | NoMain
    | VarNotFound String
    | WrongKind Kind Kind
    | NotKnArr Kind
    | WrongType Type Type
    | NotTyArr Type
    | NotForall Type
    deriving (Eq, Show)

type ThrowsError = Except LangErr

makeLenses ''Env

ctxLookup :: Bindable a => String -> Ctx a -> ThrowsError a
ctxLookup var ctx = case Map.lookup var ctx of
    (Just val) -> return val
    Nothing -> throwError $ VarNotFound var

tyLookup name = ctxLookup name . view typeCtx 
knLookup name = ctxLookup name . view kindCtx

ctxInsert ctx name v = over ctx (Map.insert name v)

insertType = ctxInsert typeCtx
insertKind = ctxInsert kindCtx

insertBoth name ty kn = insertType name ty . insertKind name kn
    

type Constrait = (Type, Type)

type Names = [String]

--type Typing = ReaderT Env WriterT [Constrait] ThrowsError
type Typing = RWST Env [Constrait] Names ThrowsError

letters = [1..] >>= flip replicateM ['A'..'Z']

fresh :: Typing Type
fresh = do
    (v : rest) <- get
    put rest
    return $ TyVar v

emptyCtx :: Env
emptyCtx = Env Map.empty Map.empty

parseError x = error $ show x

