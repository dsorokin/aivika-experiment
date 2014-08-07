
-- |
-- Module     : Simulation.Aivika.Experiment.SamplingStatsSource
-- Copyright  : Copyright (c) 2013-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It represents an optimized source of statistical data.
--

module Simulation.Aivika.Experiment.SamplingStatsSource
       (SamplingStatsSource,
        resultItemToDoubleStatsSource,
        resultItemToIntStatsSource,
        SamplingStatsData,
        samplingStatsSourceData,
        addDataToSamplingStats) where

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types

-- | Represents the optimized source of data for the statistics.
data SamplingStatsSource a = SingleValueSource (Event a)
                           | MultipleValueSource (Event (SamplingStats a))

-- | Represents the optimized data to be included in the statistics.
data SamplingStatsData a = SingleValueData !a
                         | MultipleValueData (SamplingStats a)

-- | Try to return the source of statistical data by the specified result item.
resultItemToDoubleStatsSource :: ResultItem -> Maybe (SamplingStatsSource Double)
resultItemToDoubleStatsSource item =
  case resultItemToDouble item of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case resultItemToDoubleStats item of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing
        
-- | Try to return the source of statistical data by the specified result item.
resultItemToIntStatsSource :: ResultItem -> Maybe (SamplingStatsSource Int)
resultItemToIntStatsSource item =
  case resultItemToInt item of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case resultItemToIntStats item of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing

-- | Get data from the source in the current time point.
samplingStatsSourceData :: SamplingStatsSource a -> Event (SamplingStatsData a)
samplingStatsSourceData source =
  case source of
    SingleValueSource x -> x >>= return . SingleValueData
    MultipleValueSource x -> x >>= return . MultipleValueData

-- | Add data from the source to the statistics.
addDataToSamplingStats :: SamplingData a
                          => SamplingStatsData a
                          -> SamplingStats a
                          -> SamplingStats a
addDataToSamplingStats d stats =
  case d of
    SingleValueData x -> addSamplingStats x stats
    MultipleValueData x -> combineSamplingStats x stats
