module Main where

import Control.Monad.Except
import Control.Monad.RWS
import System.Environment
import Data.Foldable

import qualified Data.Map.Strict as Map

import Parser
import Syntax
import Type
import Kind

main :: IO ()
main = do
    src <- fmap head getArgs >>= readFile
    let parsed = parseDefs src
    case runExcept $ run parsed of
        (Right res) -> print res
        (Left err) -> print err

run :: [(String, Either Type Term)] -> ThrowsError Type
run defs = do
    ran <- evalRWST (typeExprs $ reverse defs) emptyCtx letters
    return $ fst ran

-- This stuff is really pretty hacky.

getMain :: TyCtx -> ThrowsError Type
getMain ctx = case Map.lookup "main" ctx of
    (Just main) -> return main
    Nothing -> throwError NoMain

tyIfNotAlready :: Either Type Term -> Typing (Lifetime, Type)
tyIfNotAlready (Right term) = ty term
tyIfNotAlready (Left typ) = return typ

typeExprs :: [(String, Either Type Term)] -> Typing (Lifetime, Type)
typeExprs [] = do
    envs <- ask 
    lift $ getMain (_typeCtx envs)
typeExprs ((name, rhs) : rest) = do
    ty <- tyIfNotAlready rhs 
    kn <- kind ty
    local (insertBoth name ty kn) (typeExprs rest)
