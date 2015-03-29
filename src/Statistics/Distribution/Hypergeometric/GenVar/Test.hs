{-# LANGUAGE ScopedTypeVariables #-}

module Statistics.Distribution.Hypergeometric.GenVar.Test where

import Data.Word
import Data.Int

import Statistics.Distribution.Hypergeometric.GenVar

import System.Random.MWC (initialize, GenIO)

import Data.Vector (fromList)

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Monadic

import Distribution.TestSuite.QuickCheck

testArbitraryRandom :: (GenIO -> IO Bool) -> [Word32] -> Property
testArbitraryRandom f a = monadicIO $ do
  b <- run $ initialize (fromList a) >>= f
  assert b 

tests :: IO [Test]
tests = return

  [ testGroup "identities"

    [ testGroup "small"

      [ testProperty "draw nil" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getSmall . getPositive) p :: Int64
        v <- genVar (0, i, i) g
        return $ v == 0

      , testProperty "draw all" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getSmall . getPositive) p :: Int64
        v <- genVar (i, i, i) g
        return $ v == i

      ]

    , testGroup "large"

      [ testProperty "draw nil" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getLarge . getPositive) p :: Int64
        v <- genVar (0, i, i) g
        return $ v == 0

      , testProperty "draw all" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getLarge . getPositive) p :: Int64
        v <- genVar (i, i, i) g
        return $ v == i

      ]

    ]

  ]
