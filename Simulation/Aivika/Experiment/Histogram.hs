
-- |
-- Module     : Simulation.Aivika.Experiment.Histogram
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module computes the histogram by the 
-- specified data and strategy applied for such computing.
--
-- The code in this module is essentially based on the 
-- <http://hackage.haskell.org/package/Histogram> package
-- by Mike Izbicki, who kindly agreed to re-license 
-- his library under BSD3, which allowed me to use his 
-- code and comments with some modifications. 

module Simulation.Aivika.Experiment.Histogram 
    (-- * Creating Histograms
     Histogram(..), 
     histogram, 
     histogramBinSize, 
     histogramNumBins,
     -- * Binning Strategies
     BinningStrategy(..),
     binSturges, 
     binDoane, 
     binSqrt, 
     binScott) where

import Data.List
import Data.Monoid
import qualified Data.Map as Map

import Numeric

-------------------------------------------------------------------------------

-- | Holds all the information needed to plot the histogram 
-- for a list of different series. Each series produces its 
-- own item in the resuling @[Int]@ list that may contain
-- zeros.
type Histogram = [(Double, [Int])]

-------------------------------------------------------------------------------
-- Bin counters; check out http://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width

-- | The strategy applied to calculate the histogram bins.
type BinningStrategy = [Double] -> Int

-- | This is only a helper function to convert strategies that 
-- specify bin width into strategies that specify the number of bins.
stratFromBinWidth :: [Double] -> Double -> Int
stratFromBinWidth xs h = ceiling $ (maximum xs - minimum xs) / h

-- | Sturges' binning strategy is the least computational work, 
-- but recommended for only normal data.
binSturges :: BinningStrategy
binSturges xs = ceiling $ logBase 2 n + 1
    where n = fromIntegral $ length xs
          
-- | Doane's binning strategy extends Sturges' for non-normal data.  
-- It takes a little more time because it must calculate the kurtosis 
-- (peakkiness) of the distribution.
binDoane :: BinningStrategy
binDoane xs = ceiling $ 1 + log n + log (1 + a * ((n/6)**(1/2)))
    where 
        n = fromIntegral $ length xs
        a = kurtosis xs
        
-- | Using the sqrt of the number of samples is not supported by any 
-- theory, but is commonly used by excel and other histogram making software.
binSqrt :: BinningStrategy
binSqrt xs = round $ sqrt n
    where 
        n = fromIntegral $ length xs

-- | Scott's rule is the optimal solution for normal data, but requires 
-- more computation than Sturges'.
binScott :: BinningStrategy
binScott xs = stratFromBinWidth xs $ 3.5 * stddev xs / (n**(1/3))
    where 
        n = fromIntegral $ length xs
        
-------------------------------------------------------------------------------
-- create the histogram data

-- | Creates a histogram by specifying the list of series.  Call it with one of 
-- the binning strategies that is appropriate to the type of data you have.  
-- If you don't know, then try using 'binSturges'.
histogram :: BinningStrategy -> [[Double]] -> Histogram
histogram strat xs = histogramNumBins (strat $ concat xs) xs

-- | Create a histogram by specifying the exact bin size. 
-- You probably don't want to use this function, and should use histogram 
-- with an appropriate binning strategy.
histogramBinSize :: Double -> [[Double]] -> Histogram
histogramBinSize size = combineBins . map (histBins size . bin size)

-- | Create a histogram by the specified approximated number of bins.
-- You probably don't want to use this function, and should use 
-- histogram with an appropriate binning strategy.
histogramNumBins :: Int -> [[Double]] -> Histogram
histogramNumBins n xs =
  histogramBinSize size xs
    where
        size = fromIntegral (firstdigit diff) * (10 ** fromIntegral (exponent10 diff))
        diff = if diff_test > 0
               then diff_test
               else 1
        diff_test = (maximum (map maximum xs) - 
                     minimum (map minimum xs)) / fromIntegral (max 1 n)

        firstdigit dbl = floor $ dbl / (10 ** fromIntegral (exponent10 dbl))
        exponent10 dbl = floor $ logBase 10 dbl

-- | It does all the binning for the histogram.
histBins :: Double -> [Double] -> [(Double, Int)]
histBins size xs = [ (head l, length l) | l <- group (sort xs) ]

-- | It "rounds" every number into the closest number below it 
-- that is divisible by size.
bin :: Double -> [Double] -> [Double]
bin size xs@[_] = xs
bin size xs = map (\x -> size * fromIntegral (floor (x / size)) + size / 2) xs

-- | Combine bins from different histograms (optimized version).
combineBins :: [[(Double, Int)]] -> [(Double, [Int])]
combineBins [xs] = map (\(t, n) -> (t, [n])) xs
combineBins xss  = combineBins' xss

-- | Combine bins from different histograms (generic version).
combineBins' :: [[(Double, Int)]] -> [(Double, [Int])]
combineBins' xs = map (merging . sorting) $ 
                  groupping $ ordering $ concat $ numbering xs
  where numbering       = zipWith (curry indexing) [1..]
        indexing (i, x) = map (\(t, n) -> (i, t, n)) x
        ordering  = sortBy  $ \(_, t1, _) (_, t2, _) -> compare t1 t2
        groupping = groupBy $ \(_, t1, _) (_, t2, _) -> t1 == t2
        sorting   = sortBy  $ \(i1, _, _) (i2, _, _) -> compare i1 i2
        merging zs@((_, t, _) : _) = merging' zs t 1 []
        merging' [] t k acc
          | k <= count = merging' [] t (k + 1) (0 : acc)
          | k > count  = (t, reverse acc)
        merging' zs@((i, _, n) : xs) t k acc
          | i == k = merging' xs t (k + 1) (n : acc)
          | i > k  = merging' zs t (k + 1) (0 : acc)
        count = length xs

------------------------------------------------------------------------------
-- simple math functions
-- taken from package hstats, which wouldn't fully compile

-- | Numerically stable mean.
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- | Standard deviation of sample.
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- | Sample variance.
var :: (Floating a) => [a] -> a
var xs = var' 0 0 0 xs / fromIntegral (length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta / fromIntegral (n + 1)

-- | kurtosis is taken from wikipedia's definition.
kurtosis :: (Floating a) => [a] -> a
kurtosis xs = ((1/n) * sum [(x-x_bar)^4 | x <- xs])
            / ((1/n) * sum [(x-x_bar)^2 | x <- xs])^2 -3
    where 
        n = fromIntegral $ length xs
        x_bar = mean xs
