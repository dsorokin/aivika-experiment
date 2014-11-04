
-- |
-- Module     : Simulation.Aivika.Experiment.SamplingStatsWriter
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'SamplingStatsWriter' that knows how to write
-- the sampling statistics in HTML.
--

module Simulation.Aivika.Experiment.SamplingStatsWriter 
       (SamplingStatsWriter(..),
        defaultSamplingStatsWriter) where

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter

import Simulation.Aivika.Statistics

-- | Defines a writer that knows how to represent the 'SamplingStats'
-- as the HTML table.
data SamplingStatsWriter a =
  SamplingStatsWriter { samplingStatsWidth         :: Int,
                        -- ^ The width of the HTML table.
                        samplingStatsMeanText      :: String,
                        -- ^ Translated text \"mean\".
                        samplingStatsDeviationText :: String,
                        -- ^ Translated text \"deviation\".
                        samplingStatsMinText       :: String,
                        -- ^ Translated text \"minimum\".
                        samplingStatsMaxText       :: String,
                        -- ^ Translated text \"maximum\".
                        samplingStatsCountText       :: String,
                        -- ^ Translated text \"count\".
                        samplingStatsFormatter     :: ShowS,
                        -- ^ The formatter of numbers.
                        samplingStatsWrite         :: SamplingStatsWriter a -> String ->
                                                      SamplingStats a -> HtmlWriter ()
                        -- ^ This function reprensents the named statistics
                        -- as the HTML table.
                      }

-- | The default writer.
defaultSamplingStatsWriter :: Show a => SamplingStatsWriter a
defaultSamplingStatsWriter =
  SamplingStatsWriter { 
    samplingStatsWidth = 400,
    samplingStatsMeanText = "mean",
    samplingStatsDeviationText = "deviation",
    samplingStatsMinText = "minimum",
    samplingStatsMaxText = "maximum",
    samplingStatsCountText = "count",
    samplingStatsFormatter = id,
    samplingStatsWrite = \writer name stats ->
      do let format x = samplingStatsFormatter writer x
         writeHtml "<p>"
         writeHtml "<table frame='border' cellspacing='4' width='"
         writeHtml $ show $ samplingStatsWidth writer
         writeHtml "'>"
         writeHtml "<tr>"
         writeHtml "<td colspan='2'>"
         writeHtml "<p align='center'>"
         writeHtmlText name
         writeHtml "</h4>"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ samplingStatsMeanText writer 
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ samplingStatsMean stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ samplingStatsDeviationText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ samplingStatsDeviation stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ samplingStatsMinText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ samplingStatsMin stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ samplingStatsMaxText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ samplingStatsMax stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ samplingStatsCountText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtml $ format $ show $ samplingStatsCount stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "</table>" 
         writeHtml "</p>"
    }
