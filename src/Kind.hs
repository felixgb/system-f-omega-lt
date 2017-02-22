module Kind where

import Control.Monad.Except

import Syntax

type KnCtx = Ctx Kind

isKnStar :: Type -> KnCtx -> ThrowsError ()
isKnStar ty ctx = kind ty ctx >>= \(k, _) -> unless (k == KnStar) (throwError $ WrongKind KnStar k)

kind :: Type -> KnCtx -> ThrowsError (Kind, KnCtx)
kind ty ctx = case ty of
    TyInt -> return (KnStar, ctx)

    (TyVar x) -> ctxLookup x ctx >>= \k -> return (k, ctx)

    (TyArr t1 t2) -> do
        isKnStar t1 ctx
        isKnStar t2 ctx
        return (KnStar, ctx)

    (Forall var kn t1) -> do
        let newCtx = ctxInsert var kn ctx
        isKnStar t1 newCtx
        return (KnStar, ctx)

    (OpLam var kn t1 clos) -> do
        let newCtx = ctxInsert var kn ctx
        (k2, _) <- kind t1 newCtx
        return $ (KnArr kn k2, ctx)

    (OpApp t1 t2) -> do
        ((KnArr k11 k12), _) <- kind t1 ctx
        (k2, _) <- kind t2 ctx
        unless (k2 == k11) (throwError $ WrongKind k2 k11)
        return (k12, ctx)
