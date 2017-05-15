import Test.Hspec

import Control.Monad.Except
import Control.Exception

import Run
import Syntax

main :: IO ()
main = mapM_ hspec [ sysFTests
                   ]
sysFTests = do
    describe "System F Tests" $ do
        it "tests polymorphic identity function applied to function type" $ do
            source <- readFile "testfiles/test1.calc"
            fromThrowsError (run source) `shouldBe` TyInt

        it "tests polymorphic identity function applied to integers" $ do
            source <- readFile "testfiles/test2.calc"
            fromThrowsError (run source) `shouldBe` TyInt

        it "tests polymorphic double application function" $ do
            source <- readFile "testfiles/test3.calc"
            fromThrowsError (run source) `shouldBe` (Forall "X" KnStar (TyArr (TyArr (TyVar "X") (TyVar "X")) (TyArr (TyVar "X") (TyVar "X"))))

        it "tests polymorphic double application function applied to Int type" $ do
            source <- readFile "testfiles/test4.calc"
            fromThrowsError (run source) `shouldBe` (TyArr (TyArr TyInt TyInt) (TyArr TyInt TyInt))

        it "encodes boolean types in system F" $ do
            source <- readFile "testfiles/test5.calc"
            fromThrowsError (run source) `shouldBe` (Forall "X" KnStar (TyArr (TyVar "X") (TyArr (TyVar "X") (TyVar "X"))))

        it "tests polymorphic first class type constructor" $ do
            source <- readFile "testfiles/test6.calc"
            fromThrowsError (run source) `shouldBe` (Forall "X" KnStar (TyVar "X"))

        it "tests polymorphic self application. not sure about this one." $ do
            source <- readFile "testfiles/test7.calc"
            fromThrowsError (run source) `shouldBe` (TyArr (Forall "X" KnStar (TyArr (TyVar "X") (TyVar "X"))) (Forall "X" KnStar (TyArr (TyVar "X") (TyVar "X"))))

        it "tests application of double function to several types" $ do
            source <- readFile "testfiles/test8.calc"
            fromThrowsError (run source) `shouldBe` (Forall "X" KnStar (TyArr (TyArr (TyVar "X") (TyVar "X")) (TyArr (TyVar "X") (TyVar "X"))))

        it "encodes the natural numbers in system F, zero, succ, etx, with types" $ do
            source <- readFile "testfiles/test9.calc"
            fromThrowsError (run source) `shouldBe` (Forall "X" KnStar (TyArr (TyArr (TyVar "X") (TyVar "X")) (TyArr (TyVar "X") (TyVar "X"))))

fromThrowsError :: ThrowsError a -> a
fromThrowsError err = case runExcept err of
    Right ok -> ok
    Left bad -> error (show bad)
