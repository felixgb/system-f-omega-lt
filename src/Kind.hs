module Kind where

import Control.Monad.Except
import Control.Monad.Reader

import Syntax

type KnCtx = Ctx Kind

getKnArr :: Kind -> ThrowsError (Kind, Kind)
getKnArr (KnArr k1 k2) = return (k1, k2)
getKnArr k = throwError $ NotKnArr k

isKnStar :: Type -> Typing ()
isKnStar ty = kind ty >>= \k -> unless (k == KnStar) (throwError $ WrongKind KnStar k)

kind :: Type -> Typing Kind
kind ty = case ty of
    TyInt -> return KnStar

    (TyVar x) -> ask >>= lift . knLookup x

    (TyArr t1 t2) -> 
        isKnStar t1 >>
        isKnStar t2 >>
        return KnStar

    (Forall var kn t1) ->
        local (insertKind var kn) (isKnStar t1) >>
        return KnStar

    (OpLam var kn t1 clos) ->
        local (insertKind var kn) (kind t1) >>=
        return . KnArr kn

    (OpApp t1 t2) -> do
        (k11, k12) <- kind t1 >>= lift . getKnArr
        k2 <- kind t2
        unless (k2 == k11) (throwError $ WrongKind k2 k11)
        return k12
