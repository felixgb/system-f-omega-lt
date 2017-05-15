module Run where

import Control.Monad.Except
import Control.Monad.RWS
import System.Environment
import Data.Foldable

import qualified Data.Map.Strict as Map

import Parser
import Syntax
import Type
import Kind

run :: String -> ThrowsError Type
run source = fromParsed $ parseDefs source

fromParsed :: [(String, Either Type Term)] -> ThrowsError Type
fromParsed defs = do
    ran <- evalRWST (typeExprs $ reverse defs) emptyCtx letters
    return $ fst ran

getMain :: TyCtx -> ThrowsError Type
getMain ctx = case Map.lookup "main" ctx of
    (Just main) -> return main
    Nothing -> throwError NoMain

tyIfNotAlready :: Either Type Term -> Typing Type
tyIfNotAlready (Right term) = ty term
tyIfNotAlready (Left typ) = return typ

typeExprs :: [(String, Either Type Term)] -> Typing Type
typeExprs [] = do
    envs <- ask 
    lift $ getMain (_typeCtx envs)
typeExprs ((name, rhs) : rest) = do
    ty <- tyIfNotAlready rhs 
    kn <- kind ty
    local (insertBoth name ty kn) (typeExprs rest)
