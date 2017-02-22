module Main where

import Control.Monad.Except
import System.Environment
import Data.Foldable

import qualified Data.Map.Strict as Map

import Parser
import Syntax
import Type

main :: IO ()
main = do
    src <- fmap head getArgs >>= readFile
    let parsed = parseDefs src
    case runExcept $ run parsed of
        (Right res) -> print res
        (Left err) -> print err

getMain :: [(String, Either Type Term)] -> ThrowsError (Either Type Term)
getMain ctx = case lookup "main" ctx of
    (Just main) -> return main
    Nothing -> throwError NoMain

run :: [(String, Either Type Term)] -> ThrowsError Type
run defs = do
    tyDefs <- foldrM (\(x, t) ctx -> tyIfNotAlready t ctx >>= \tyI -> return $ ctxInsert x tyI ctx) emptyCtx defs
    mn <- getMain defs
    case mn of
        (Right trm) -> ty trm tyDefs emptyCtx
        (Left typ) -> return typ

tyIfNotAlready :: Either Type Term -> TyCtx -> ThrowsError Type
tyIfNotAlready (Right term) ctx = ty term ctx (Map.singleton "CBool" $ KnStar)
tyIfNotAlready (Left typ) _ = return typ
    
