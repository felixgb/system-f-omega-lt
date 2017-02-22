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

getMain :: [(String, Term)] -> ThrowsError Term
getMain ctx = case lookup "main" ctx of
    (Just main) -> return main
    Nothing -> throwError NoMain

run defs = do
    tyDefs <- foldrM (\(x, t) map -> ty t map emptyCtx >>= \tyI -> return $ ctxInsert x tyI map) emptyCtx defs
    main <- getMain defs
    ty main tyDefs emptyCtx
