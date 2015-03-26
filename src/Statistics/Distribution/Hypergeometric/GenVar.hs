{-# LANGUAGE Trustworthy #-}
-- |
-- Module    : Statistics.Distribution.Hypergeometric.GenVar
-- Copyright : (c) 2015 Sam Rijs
--             (c) 2005 Robert Kern
--             (c) 1998 Ivan Frohne
-- License   : MIT
--
-- Maintainer  : srijs@airpost.net
-- Stability   : experimental
--

module Statistics.Distribution.Hypergeometric.GenVar (genVar) where

import Control.Monad.Primitive (PrimMonad, PrimState)

import Statistics.Distribution (ContGen(..), DiscreteGen(..))
import Statistics.Distribution.Hypergeometric (HypergeometricDistribution, hdM, hdL, hdK)

import Numeric.SpecFunctions (logGamma)

import System.Random.MWC (Gen, uniform)

d1 = 1.7155277699214135 -- 2*sqrt(2/e)
d2 = 0.8989161620588988 -- 3 - 2*sqrt(3/e)

instance ContGen HypergeometricDistribution
  where genContVar d g = return . fromIntegral =<< genVar (hdM d, hdL d, hdK d) g

instance DiscreteGen HypergeometricDistribution
  where genDiscreteVar d g = genVar (hdM d, hdL d, hdK d) g 

-- | Random variates from the hypergeometric distribution /(m, l, k)/.
-- Returns the number of white balls drawn when /k/ balls
-- are drawn at random from an urn containing /m/ white
-- and /l-m/ black balls.
genVar :: (PrimMonad m, Integral a) => (a, a, a) -> Gen (PrimState m) -> m a
genVar (good, popsize, sample) gen = return . fix2 . fix1 =<< loop
  where fix1 z = if good > bad then m - z else z
        fix2 z = if m < sample then good - z else z
        mingoodbad = min good bad
        bad = popsize - good
        maxgoodbad = max good bad
        m = min sample (popsize - sample)
        gamma z = sum $ map (logGamma . fromIntegral)
          [ z + 1,     mingoodbad - z + 1
          , m - z + 1, maxgoodbad - m + z + 1 ]
        d4 = fromIntegral mingoodbad / fromIntegral popsize
        d5 = 1 - d4
        d6 = fromIntegral m * d4 + 0.5
        d7 = sqrt $ fromIntegral (popsize - m) * fromIntegral sample * d4 * d5 / fromIntegral (popsize - 1) + 0.5
        d8 = d1 * d7 + d2
        d9 = floor $ fromIntegral (m + 1) * fromIntegral (mingoodbad + 1) / fromIntegral (popsize + 2)
        d10 = gamma d9
        d11 = fromIntegral $ min (min m mingoodbad + 1) (floor (d6 + 16 * d7))
        -- 16 for 16-decimal-digit precision in d1 and d2
        loop = do
          x <- uniform gen
          y <- uniform gen
          case () of 
            _  | w < 0 || w >= d11    -> loop -- fast rejection
               | x * (4 - x) - 3 <= t -> return z -- fast acceptance
               | x * (x - t) >= 1     -> loop -- fast rejection
               | 2 * (log x) <= t     -> return z -- acceptance
               | otherwise            -> loop -- rejection
               where w = d6 + d8 * (y - 0.5) / x
                     z = floor w
                     t = d10 - gamma z
