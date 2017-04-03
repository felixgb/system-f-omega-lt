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

s2 :: Type -> Typing Type
s2 (OpApp (OpLam name kn body _) t2) = s2 $ apply (Subst $ Map.singleton name t2) body
s2 (OpApp t1 t2) = s2 t2
s2 fa@(Forall _ _ _) = return fa
s2 other = error $ show other

ty :: Term -> Typing Type
ty term = case term of
    (Lit n) -> return TyInt

    (Var name) -> ask >>= lift . tyLookup name

    (Lam var t1 body clos) -> do
        isKnStar t1
        bodyTy <- local (insertType var t1) (ty body)
        return $ TyArr t1 bodyTy

    (App t1 t2) -> do
        (t11, t12) <- ty t1 >>= lift . getTyArr
        ty2 <- ty t2
        areEq <- ty2 `tyEq` t11
        unless areEq (throwError $ WrongType ty2 t11)
        return t12

    (TyLam var (kn, _) body clos) -> do
        t <- local (insertKind var kn) (ty body)
        return $ Forall var kn t

    (TyApp t1 argTy) -> do
        (var, kn, ty2) <- ty t1 >>= simplify >>= lift . getForall
        tyK <- kind argTy
        unless (tyK == kn) (throwError $ WrongKind tyK kn)
        return $ apply (Subst $ Map.singleton var argTy) ty2

simplify :: Type -> Typing Type
simplify (TyVar name) = ask >>= lift . tyLookup name
simplify other = return other

tyEq :: Type -> Type -> Typing Bool
tyEq fa@(Forall v1 k1 t1) (TyVar name) = do
    env <- ask
    r <- lift $ tyLookup name env
    return (r == fa)
tyEq a b = return $ a == b
