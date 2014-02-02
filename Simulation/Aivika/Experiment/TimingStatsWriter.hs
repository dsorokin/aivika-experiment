
-- |
-- Module     : Simulation.Aivika.Experiment.TimingStatsWriter
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'TimingStatsWriter' that knows how to write
-- the timing statistics in HTML.
--

module Simulation.Aivika.Experiment.TimingStatsWriter 
       (TimingStatsWriter(..),
        defaultTimingStatsWriter) where

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter

import Simulation.Aivika.Statistics

-- | Defines a writer that knows how to represent the 'TimingStats'
-- as the HTML table.
data TimingStatsWriter a =
  TimingStatsWriter { timingStatsWidth         :: Int,
                      -- ^ The width of the HTML table.
                      timingStatsTimeText      :: String,
                      -- ^ Translated text \"time\".
                      timingStatsMeanText      :: String,
                      -- ^ Translated text \"mean\".
                      timingStatsDeviationText :: String,
                      -- ^ Translated text \"deviation\".
                      timingStatsMinText       :: String,
                      -- ^ Translated text \"minimum\".
                      timingStatsMaxText       :: String,
                      -- ^ Translated text \"maximum\".
                      timingStatsFormatter     :: ShowS,
                      -- ^ The formatter of numbers.
                      timingStatsWrite         :: TimingStatsWriter a -> String ->
                                                  TimingStats a -> HtmlWriter ()
                      -- ^ This function reprensents the named statistics
                      -- as the HTML table.
                      }

-- | The default writer.
defaultTimingStatsWriter :: (Show a, TimingData a) => TimingStatsWriter a
defaultTimingStatsWriter =
  TimingStatsWriter { 
    timingStatsWidth = 400,
    timingStatsMeanText = "mean",
    timingStatsTimeText = "time",
    timingStatsDeviationText = "deviation",
    timingStatsMinText = "minimum",
    timingStatsMaxText = "maximum",
    timingStatsFormatter = id,
    timingStatsWrite = \writer name stats ->
      do let format x = timingStatsFormatter writer (show x)
         writeHtml "<p>"
         writeHtml "<table frame='border' cellspacing='4' width='"
         writeHtml $ show $ timingStatsWidth writer
         writeHtml "'>"
         writeHtml "<tr>"
         writeHtml "<td colspan='3'>"
         writeHtml "<p align='center'>"
         writeHtmlText name
         writeHtml "</h4>"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ timingStatsMeanText writer 
         writeHtml "</td>"
         writeHtml "<td colspan='2'>"
         writeHtmlText $ format $ timingStatsMean stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ timingStatsDeviationText writer
         writeHtml "</td>"
         writeHtml "<td colspan='2'>"
         writeHtmlText $ format $ timingStatsDeviation stats
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ timingStatsMinText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ timingStatsMin stats
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText "("
         writeHtmlText $ timingStatsTimeText writer
         writeHtmlText " = "
         writeHtmlText $ format $ timingStatsMinTime stats
         writeHtmlText ")"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ timingStatsMaxText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ timingStatsMax stats
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText "("
         writeHtmlText $ timingStatsTimeText writer
         writeHtmlText " = "
         writeHtmlText $ format $ timingStatsMaxTime stats
         writeHtmlText ")"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ timingStatsTimeText writer
         writeHtml "</td>"
         writeHtml "<td colspan='2'>"
         writeHtml "["
         writeHtml $ format $ timingStatsStartTime stats
         writeHtml "; "
         writeHtml $ format $ timingStatsLastTime stats
         writeHtml "]"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "</table>" 
         writeHtml "</p>"
    }
