import Test.HUnit

import Control.Monad.Except
import Control.Monad.RWS
import Data.Foldable

import Parser
import Syntax

main = runTestTT tests

parseBodge :: String -> ThrowsError Term
parseBodge = return . parseDefs

tests = TestList
    [ TestLabel "foo test" (testProgram "id" "testfiles/lt_tests/test1.calc" parseBodge (Var "x"))
    ]

failExcept :: (Show a, Eq a, Bindable a) => ThrowsError a -> (a -> Assertion) -> Assertion
failExcept val testf = case runExcept val of
    Right good -> testf good
    Left err -> assertFailure (show err)

testProgram :: (Show a, Eq a, Bindable a) => String -> FilePath -> (String -> ThrowsError a) -> a -> Test
testProgram name path func expected = TestCase $ do
    source <- readFile path
    failExcept (func source) assertion
    where assertion v = assertEqual name v expected

testId = TestCase $ assertEqual "Should do a thing" (show $ parseTerm "x") "x"

testFoo = TestCase $ do
    putStrLn "hello"
    assertEqual "sfsdfsdf" 2 1

run :: [(String, Either Type Term)] -> ThrowsError Type
run defs = do
    ran <- evalRWST (typeExprs $ reverse defs) emptyCtx letters
    return $ fst ran

-- This stuff is really pretty hacky.

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
