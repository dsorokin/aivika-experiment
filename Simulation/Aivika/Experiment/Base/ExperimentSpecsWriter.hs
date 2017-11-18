
-- |
-- Module     : Simulation.Aivika.Experiment.Base.ExperimentSpecsWriter
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'ExperimentSpecsWriter' that knows how to write
-- in HTML the experiment specs which include the simulation specs and 
-- the number of simulation runs.
--

module Simulation.Aivika.Experiment.Base.ExperimentSpecsWriter 
       (ExperimentSpecsWriter(..),
        defaultExperimentSpecsWriter) where

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.HtmlWriter

-- | Defines a writer that knows how to represent the
-- experiment specs as the HTML table.
data ExperimentSpecsWriter =
  ExperimentSpecsWriter { experimentSpecsWidth         :: Int,
                          -- ^ The width of the HTML table.
                          experimentSpecsNameText     :: String,
                          -- ^ Translated text \"Experiment Specs\".
                          experimentSpecsStartTimeText :: String,
                          -- ^ Translated text \"start time\".
                          experimentSpecsStopTimeText  :: String,
                          -- ^ Translated text \"stop time\".
                          experimentSpecsDTText        :: String,
                          -- ^ Translated text \"time step\".
                          experimentSpecsRunCountText  :: String,
                          -- ^ Translated text \"run count\".
                          experimentSpecsIntegMethodText    :: String,
                          -- ^ Translated text \"integration method\".
                          experimentSpecsEulerText     :: String,
                          -- ^ Translated text \"Euler's\".
                          experimentSpecsRungeKutta2Text :: String,
                          -- ^ Translated text \"the 2-nd order Runge-Kutta\".
                          experimentSpecsRungeKutta4Text :: String,
                          -- ^ Translated text \"the 4-th order Runge-Kutta\".
                          experimentSpecsRungeKutta4bText :: String,
                          -- ^ Translated text \"the 4-th order Runge-Kutta 3/8-rule\".
                          experimentSpecsFormatter     :: ShowS,
                          -- ^ The formatter of numbers.
                          experimentSpecsWrite :: ExperimentSpecsWriter
                                                  -> Experiment
                                                  -> HtmlWriter ()
                          -- ^ This function creates HTML.
                        }

-- | The default writer.
defaultExperimentSpecsWriter :: ExperimentSpecsWriter
defaultExperimentSpecsWriter =
  ExperimentSpecsWriter { 
    experimentSpecsWidth = 400,
    experimentSpecsNameText = "Experiment Specs",
    experimentSpecsStartTimeText = "start time",
    experimentSpecsStopTimeText = "stop time",
    experimentSpecsDTText = "time step",
    experimentSpecsRunCountText = "run count",
    experimentSpecsIntegMethodText = "integration method",
    experimentSpecsEulerText = "Euler's",
    experimentSpecsRungeKutta2Text = "the 2-nd order Runge-Kutta",
    experimentSpecsRungeKutta4Text = "the 4-th order Runge-Kutta",
    experimentSpecsRungeKutta4bText = "the 4-th order Runge-Kutta 3/8-rule",
    experimentSpecsFormatter = id,
    experimentSpecsWrite = \writer exp ->
      do let format x = experimentSpecsFormatter writer x
         writeHtml "<p>"
         writeHtml "<table frame='border' cellspacing='4' width='"
         writeHtml $ show $ experimentSpecsWidth writer
         writeHtml "'>"
         writeHtml "<tr>"
         writeHtml "<td colspan='2'>"
         writeHtml "<p align='center'>"
         writeHtmlText $ experimentSpecsNameText writer
         writeHtml "</p>"
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ experimentSpecsStartTimeText writer 
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ spcStartTime $ experimentSpecs exp
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ experimentSpecsStopTimeText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ spcStopTime $ experimentSpecs exp
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ experimentSpecsDTText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ spcDT $ experimentSpecs exp
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ experimentSpecsRunCountText writer
         writeHtml "</td>"
         writeHtml "<td>"
         writeHtmlText $ format $ show $ experimentRunCount exp
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "<tr>"
         writeHtml "<td>"
         writeHtmlText $ experimentSpecsIntegMethodText writer
         writeHtml "</td>"
         writeHtml "<td>"
         let method = spcMethod $ experimentSpecs exp
         writeHtml $ methodName method writer
         writeHtml "</td>"
         writeHtml "</tr>"
         writeHtml "</table>" 
         writeHtml "</p>"
    }

-- | Return the method name.
methodName :: Method -> ExperimentSpecsWriter -> String
methodName Euler       = experimentSpecsEulerText
methodName RungeKutta2 = experimentSpecsRungeKutta2Text
methodName RungeKutta4 = experimentSpecsRungeKutta4Text
methodName RungeKutta4b = experimentSpecsRungeKutta4bText
