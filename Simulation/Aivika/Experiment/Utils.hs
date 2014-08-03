
-- |
-- Module     : Simulation.Aivika.Experiment.Utils
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines utility functions missed in the standard library.
--

module Simulation.Aivika.Experiment.Utils
       (divideBy, replace) where

import Data.List
import Data.List.Split

-- | Divide into the groups removing those elements
-- that satisfy the predicate.
divideBy :: (a -> Bool) -> [a] -> [[a]]
divideBy p xs =
  case dropWhile p xs of
    []  -> []
    xs' -> ys : divideBy p xs''
           where (ys, xs'') = break p xs'

-- | Replace the string.
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old