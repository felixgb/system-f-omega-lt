module Type where

import qualified Data.Map.Strict as Map

import Control.Monad.Except

import Kind
import Syntax

type TyCtx = Ctx Type

ty :: Term -> TyCtx -> KnCtx -> ThrowsError Type
ty term ctx kctx = case term of
    (Lit n) -> return TyInt

    (Var name) -> ctxLookup name ctx 

    (Lam var t1 body clos) -> do
        isKnStar t1 kctx
        let newCtx = ctxInsert var t1 ctx
        t2 <- ty body newCtx kctx
        return (t1 `TyArr` t2)

    (App t1 t2) -> do
        (TyArr t11 t12) <- ty t1 ctx kctx
        ty2 <- ty t2 ctx kctx
        unless (ty2 == t11) (throwError $ WrongType ty2 t11)
        return t12

    (TyLam var kn body clos) -> do
        let newKctx = ctxInsert var kn kctx
        bodyTy <- ty body ctx newKctx
        return $ Forall var kn bodyTy

    (TyApp t1 argTy) -> do
        (Forall var kn ty2) <- ty t1 ctx kctx
        (tyK, _) <- kind argTy kctx
        unless (tyK == kn) (throwError $ WrongKind tyK kn)
        return $ apply (Subst $ Map.singleton var argTy) ty2

