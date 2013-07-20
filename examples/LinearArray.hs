
-- The model demonstrates using the arrays
-- as described in model Linear Array from Berkeley Madonna.
--
-- It defines only one helper function generateArray.
-- If the model used vectors (Data.Vector) then there would be
-- no need in this helper function.

{-# LANGUAGE RecursiveDo #-}

import Data.Array
import Control.Monad
import Control.Monad.Trans

import qualified Data.Vector as V

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.SystemDynamics
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Dynamics.Signal

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.LastValueView
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.FinalStatsView
import Simulation.Aivika.Experiment.TimingStatsView
import Simulation.Aivika.Experiment.FinalTableView

specs = Specs { spcStartTime = 0, 
                spcStopTime = 500, 
                spcDT = 0.1,
                spcMethod = RungeKutta4 }

-- | This is an analog of 'Data.Vector.generateM' included in the Haskell platform.
generateArray :: (Ix i, Monad m) => (i, i) -> (i -> m a) -> m (Array i a)
generateArray bnds generator =
  do ps <- forM (range bnds) $ \i ->
       do x <- generator i
          return (i, x)
     return $ array bnds ps

model :: Int -> Simulation ExperimentData
model n =
  mdo queue <- newQueue
      m <- generateArray (1, n) $ \i ->
        integ (q + k * (c!(i - 1) - c!i) + k * (c!(i + 1) - c!i)) 0
      let c =
            array (0, n + 1) [(i, if (i == 0) || (i == n + 1)
                                  then 0
                                  else (m!i / v)) | i <- [0 .. n + 1]]
          q = 1
          k = 2
          v = 0.75
      experimentDataInStartTime queue
        [("t", seriesEntity "time" time),
         ("m", seriesEntity "M" m),
         ("c", seriesEntity "C" c)]

experiment :: Experiment
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
         tableSeries = ["t", "m", "c"] },
       outputView $ defaultFinalTableView {
         finalTableSeries = ["m", "c"] }, 
       outputView $ defaultLastValueView {
         lastValueSeries = ["t", "m", "c"] },
       outputView $ defaultTimingStatsView {
         timingStatsSeries = ["t", "m", "c"] },
       outputView $ defaultFinalStatsView {
         finalStatsSeries = ["m", "c"] } ] } 

main = runExperiment experiment (model 51)
