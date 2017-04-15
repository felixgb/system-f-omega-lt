module Type where

import qualified Data.Map.Strict as Map

import Control.Monad.Except
import Control.Monad.RWS

import Kind
import Syntax

type TyCtx = Ctx Type

getTyArr :: Type -> ThrowsError (Type, Type)
getTyArr (TyArr (l1, t1) (l2, t2)) = return (t1, t2)
getTyArr t = throwError $ NotTyArr t

getForall :: Type -> ThrowsError (String, Kind, Type)
getForall (Forall var kn ty) = return (var, kn, ty)
getForall t = throwError $ NotForall t

ty :: Term -> Typing (Lifetime, Type)
ty term = case term of
    (Lit n) -> return (LiStatic, TyInt)

    (Var name) -> ask >>= lift . tyLookup name

    (Lam var (lt, t1) body) -> do
         isKnStar t1
         -- next li level
         (bodyLt, bodyTy) <- local (insertType var (lt, t1)) (ty body)
         unless (bodyLt <= lt) (throwError $ LifetimeError body)
         return $ (LiDummy, TyArr (lt, t1) (bodyLt, bodyTy))
 
    (App t1 t2) -> do
        -- when getting the function, it must have some lifetime var. Get that
        -- lifetime var and map it to the the current (???) lifetime that is
        -- being used..... is there any point in lifetime application in that
        -- case...? yeah you have to, but those are only for lifetime functions.
        -- Ok, so don't map lifetime variable in normal applition. The lifetime
        -- of an application is the lifetime of the result?
        (t11, t12) <- ty t1 >>= lift . getTyArr . snd
        (li2, ty2) <- ty t2
        areEq <- ty2 `tyEq` t11
        unless areEq (throwError $ WrongType ty2 t11)
        return (li2, t12)

--     (TyLam var kn body clos) -> do
--         t <- local (insertKind var kn) (ty body)
--         return $ Forall var kn t
-- 
--     (TyApp t1 argTy) -> do
--         (var, kn, ty2) <- ty t1 >>= simplify >>= lift . getForall
--         tyK <- kind argTy
--         unless (tyK == kn) (throwError $ WrongKind tyK kn)
--         return $ apply (Subst $ Map.singleton var argTy) ty2
-- 
--     (LiLam var term) -> ty term
-- 
--     (LiApp t1 t2) -> error "not yet implemented"
-- 
--     (Lt lt) -> error "not yet implemented"
-- 
-- simplify :: Type -> Typing Type
-- simplify (TyVar name) = ask >>= lift . tyLookup name
-- simplify other = return other
-- 
liOrd :: Lifetime -> Lifetime -> Typing Bool
liOrd (LiVar x1) (LiVar x2) = do
    env <- ask
    l1 <- liLookup x1 env
    l2 <- liLookup x2 env
    liOrd l1 l2
liOrd (LiVar x1) (LiVar x2) = do
    env <- ask
    l1 <- liLookup x1 env
    l2 <- liLookup x2 env
    liOrd l1 l2

tyEq :: Type -> Type -> Typing Bool
tyEq fa@(Forall v1 k1 t1) (TyVar name) = do
    env <- ask
    r <- lift $ fmap snd (tyLookup name env)
    return (r == fa)
tyEq a b = return $ a == b
