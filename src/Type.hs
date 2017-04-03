module Type where

import qualified Data.Map.Strict as Map

import Control.Monad.Except
import Control.Monad.RWS

import Kind
import Syntax

type TyCtx = Ctx Type

getTyArr :: Type -> ThrowsError (Type, Type)
getTyArr (TyArr t1 t2) = return (t1, t2)
getTyArr t = throwError $ NotTyArr t

getForall :: Type -> ThrowsError (String, Kind, Type)
getForall (Forall var kn ty) = return (var, kn, ty)
getForall t = throwError $ NotForall t

ty :: Term -> Typing Type
ty term = case term of
    (Lit n) -> return TyInt

    (Var name) -> ask >>= lift . tyLookup name

    (Lam var (lt, t1) body) -> do
        isKnStar t1
        bodyTy <- local (insertType var t1) (ty body)
        return $ TyArr t1 bodyTy

    (App t1 t2) -> do
        (t11, t12) <- ty t1 >>= lift . getTyArr
        ty2 <- ty t2
        areEq <- ty2 `tyEq` t11
        unless areEq (throwError $ WrongType ty2 t11)
        return t12

    (TyLam var kn body clos) -> do
        t <- local (insertKind var kn) (ty body)
        return $ Forall var kn t

    (TyApp t1 argTy) -> do
        (var, kn, ty2) <- ty t1 >>= simplify >>= lift . getForall
        tyK <- kind argTy
        unless (tyK == kn) (throwError $ WrongKind tyK kn)
        return $ apply (Subst $ Map.singleton var argTy) ty2

    (LiLam var term) -> ty term

    (LiApp t1 t2) -> error "not yet implemented"

    (Lt lt) -> error "not yet implemented"

simplify :: Type -> Typing Type
simplify (TyVar name) = ask >>= lift . tyLookup name
simplify other = return other

tyEq :: Type -> Type -> Typing Bool
tyEq fa@(Forall v1 k1 t1) (TyVar name) = do
    env <- ask
    r <- lift $ tyLookup name env
    return (r == fa)
tyEq a b = return $ a == b
