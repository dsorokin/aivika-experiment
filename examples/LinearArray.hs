
-- The model demonstrates using the arrays
-- as described in model Linear Array from Berkeley Madonna.
--
-- It defines only one helper function generateArray.
-- If the model used vectors (Data.Vector) then there would be
-- no need in this helper function.

{-# LANGUAGE RecursiveDo #-}

import Data.Array
import Data.Monoid

import Control.Monad
import Control.Monad.Trans

import qualified Data.Vector as V

import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Experiment

specs = Specs { spcStartTime = 0, 
                spcStopTime = 500, 
                spcDT = 0.1,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

-- | This is an analog of 'V.generateM' included in the Haskell platform.
generateArray :: (Ix i, Monad m) => (i, i) -> (i -> m a) -> m (Array i a)
generateArray bnds generator =
  do ps <- forM (range bnds) $ \i ->
       do x <- generator i
          return (i, x)
     return $ array bnds ps

model :: Int -> Simulation Results
model n =
  mdo m <- generateArray (1, n) $ \i ->
        integ (q + k * (c!(i - 1) - c!i) + k * (c!(i + 1) - c!i)) 0
      let c =
            array (0, n + 1) [(i, if (i == 0) || (i == n + 1)
                                  then 0
                                  else (m!i / v)) | i <- [0 .. n + 1]]
          q = 1
          k = 2
          v = 0.75
      return $ results
        [resultSource "t" "time" time,
         resultSource "m" "M" m,
         resultSource "c" "C" c]

experiment :: WebPageRendering r => Experiment r WebPageWriter
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 1,
    experimentTitle = "Linear Array",
    experimentDescription = "Model Linear Array as described in " ++
                            "the examples included in Berkeley-Madonna.",
    experimentGenerators = 
      [outputView defaultExperimentSpecsView,
       outputView $ defaultTableView {
         tableSeries =
            resultByName "t" <>
            resultByName "m" <>
            resultByName "c" },
       outputView $ defaultFinalTableView {
         finalTableSeries =
            resultByName "m" <>
            resultByName "c" },
       outputView $ defaultLastValueView {
         lastValueSeries =
            resultByName "t" <>
            resultByName "m" <>
            resultByName "c" },
       outputView $ defaultTimingStatsView {
         timingStatsSeries =
            resultByName "t" <>
            resultByName "m" <>
            resultByName "c" },
       outputView $ defaultFinalStatsView {
         finalStatsSeries =
            resultByName "m" <>
            resultByName "c" } ] }

main = runExperiment experiment WebPageRenderer (model 51)
