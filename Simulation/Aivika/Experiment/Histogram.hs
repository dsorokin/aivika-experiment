
-- | This module computes the histogram by the 
-- specified data and strategy applied for such computing.
--
-- The code in this module is almost fully based on the 
-- <http://hackage.haskell.org/package/Histogram> package
-- by Mike Izbicki, who kindly agreed to re-license 
-- his library under BSD3, which allowed me to use his 
-- code and comments almost without modifications. The 
-- main reason of such copying is that the original code 
-- depends on GnuPlot.

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
     binScott, 
     binFreedmanDiaconis) where

import Data.List
import Data.Monoid
import qualified Data.Map as Map

import Numeric

-------------------------------------------------------------------------------

-- | Holds all the information needed to plot the histogram.
type Histogram = [(Double, Int)]

-------------------------------------------------------------------------------
-- Bin counters; check out http://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width

-- | The strategy applied to calculate the histogram bins.
type BinningStrategy = [Double] -> Int

-- | This is only a helper function to convert strategies that 
-- specify bin width into strategies that specify the number of bins.
stratFromBinWidth :: [Double] -> Double -> Int
stratFromBinWidth xs h = ceiling $ ((maximum xs) - (minimum xs))/h

-- | Sturges' binning strategy is the least computational work, 
-- but recommended for only normal data.
binSturges :: BinningStrategy
binSturges xs = ceiling $ (log n)/(log 2) + 1
    where n = fromIntegral $ length xs
          
-- | Doane's binning strategy extends Sturges' for non-normal data.  
-- It takes a little more time because it must calculate the kurtosis 
-- (peakkiness) of the distribution.
binDoane :: BinningStrategy
binDoane xs = ceiling $ 1 + (log n) + (log $ 1 + a*((n/6)**(1/2)))
    where 
        n = fromIntegral $ length xs -- :: Double
        a = kurtosis xs -- :: Double
        
-- | Using the sqrt of the number of samples is not supported by any 
-- theory, but is commonly used by excel and other histogram making software.
binSqrt :: BinningStrategy
binSqrt xs = round $ sqrt n
    where 
        n = fromIntegral $ length xs

-- | Scott's rule is the optimal solution for normal data, but requires 
-- more computation than Sturges'.
binScott :: BinningStrategy
binScott xs = stratFromBinWidth xs $ 3.5*(stddev xs) / (n**(1/3))
    where 
        n = fromIntegral $ length xs
        
-- | The Freedman-Diaconis rule is less susceptible to outliers than 
-- Scott's and is also used on \"normalish\" data.
binFreedmanDiaconis :: BinningStrategy
binFreedmanDiaconis xs = stratFromBinWidth xs $ 3.5*(stddev xs) / (n**(1/3))
    where 
        n = fromIntegral $ length xs

-------------------------------------------------------------------------------
-- create the histogram data

-- | Creates a histogram that's ready for plotting.  Call it with one of 
-- the binning strategies that is appropriate to the type of data you have.  
-- If you don't know, then try using 'binSturges'.
histogram :: BinningStrategy -> [Double] -> Histogram
histogram strat xs = histogramNumBins (strat xs) xs

-- | Create a histogram by specifying the exact bin size. 
-- You probably don't want to use this function, and should use histogram 
-- with an appropriate binning strategy.
histogramBinSize :: Double -> [Double] -> Histogram
histogramBinSize size xs = fillhist size $ histbin size $ bin size xs

-- | Create a histogram by the specified number of bins.
-- You probably don't want to use this function, and should use 
-- histogram with an appropriate binning strategy.
histogramNumBins :: Int -> [Double] -> Histogram
histogramNumBins n xs =
    histogramBinSize size xs
    where
        size = (fromIntegral $ firstdigit diff) *((10) ** (fromIntegral $ exponent10 diff))
        diff = if diff_test==0
                  then 1
                  else diff_test
        diff_test = ((maximum xs)-(minimum xs))/(fromIntegral n)

        firstdigit dbl = floor $ dbl/((10) ** (fromIntegral $ exponent10 dbl))
        exponent10 dbl = floor $ log10 dbl
        log10 x = (log x) / (log 10)

-- helpers

   -- histbin does all the binning for the histogram
histbin :: Double -> [Double] -> [(Double,Int)]
histbin size xs = Map.toList $ Map.fromList [ (head l, length l) | l <- group (sort xs) ]

   -- histbin bins all the numbers in the histogram, but 
   -- it ignores any columns with zero elements.
   -- fillhist adds those zero element columns
fillhist :: Double -> [(Double,Int)] -> [(Double,Int)]
fillhist size ((a,b):[]) = [(roundFloat a,b)]
fillhist size ((a,b):xs) = 
    if abs (next-a')<0.0001
       then (roundFloat a,b):(fillhist size xs)
       else (roundFloat a,b):(fillhist size $ (next,0):xs)
    where
        a' = fst $ head xs
        b' = snd $ head xs
        next = roundFloat (a+size)

-- | bin "rounds" every number into the closest number below it 
-- that is divisible by size.
bin :: Double -> [Double] -> [Double]
bin size xs = map (\x -> size*(fromIntegral $ floor (x/size))) xs

roundFloat :: Double -> Double
roundFloat num = read $ showFFloat (Just 3) num ""

-- normalcoord m s (x,y) = normalpdf m s x

normalpdf :: Double -> Double -> Double -> Double
normalpdf m s x = (1/(s*(sqrt $ 2*pi)))*(exp $ -(x-m)^2/(2*s^2))

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
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)

-- | kurtosis is taken from wikipedia's definition.
kurtosis :: (Floating a) => [a] -> a
kurtosis xs = ((1/n) * (sum [(x-x_bar)^4 | x <- xs]))
            / ((1/n) * (sum [(x-x_bar)^2 | x <- xs]))^2 -3
    where 
        n = fromIntegral $ length xs
        x_bar = mean xs

