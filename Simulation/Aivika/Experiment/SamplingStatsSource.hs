
-- |
-- Module     : Simulation.Aivika.Experiment.SamplingStatsSource
-- Copyright  : Copyright (c) 2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It represents an optimized source of statistical data.
--

module Simulation.Aivika.Experiment.SamplingStatsSource
       (SamplingStatsSource,
        providerToDoubleStatsSource,
        providerToIntStatsSource,
        SamplingStatsData,
        samplingStatsSourceData,
        addDataToSamplingStats) where

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Statistics
import Simulation.Aivika.Experiment

-- | Represents the optimized source of data for the statistics.
data SamplingStatsSource a = SingleValueSource (Dynamics a)
                           | MultipleValueSource (Dynamics (SamplingStats a))

-- | Represents the optimized data to be included in the statistics.
data SamplingStatsData a = SingleValueData !a
                         | MultipleValueData (SamplingStats a)

-- | Try to return the source of statistical data by the specified provider.
providerToDoubleStatsSource :: SeriesProvider -> Maybe (SamplingStatsSource Double)
providerToDoubleStatsSource provider =
  case providerToDouble provider of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case providerToDoubleStats provider of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing
        
-- | Try to return the source of statistical data.
providerToIntStatsSource :: SeriesProvider -> Maybe (SamplingStatsSource Int)
providerToIntStatsSource provider =
  case providerToInt provider of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case providerToIntStats provider of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing

-- | Get data from the source in the current time point.
samplingStatsSourceData :: SamplingStatsSource a -> Dynamics (SamplingStatsData a)
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
