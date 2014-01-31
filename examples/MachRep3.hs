
-- It corresponds to model MachRep3 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Variation of models MachRep1, MachRep2. Two machines, but
-- sometimes break down. Up time is exponentially distributed with mean
-- 1.0, and repair time is exponentially distributed with mean 0.5. In
-- this example, there is only one repairperson, and she is not summoned
-- until both machines are down. We find the proportion of up time. It
-- should come out to about 0.45.

-- module MachRep3Model (model) where

import System.Random
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.LastValueView
import Simulation.Aivika.Experiment.TableView
import Simulation.Aivika.Experiment.TimingStatsView
import Simulation.Aivika.Experiment.FinalStatsView
import Simulation.Aivika.Experiment.ExperimentSpecsView
import Simulation.Aivika.Experiment.FinalTableView

specs = Specs { spcStartTime = 0.0,
                spcStopTime = 1000.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }

description =
  "Model MachRep3. Variation of models MachRep1, MachRep2. Two machines, but " ++
  "sometimes break down. Up time is exponentially distributed with mean " ++
  "1.0, and repair time is exponentially distributed with mean 0.5. In " ++
  "this example, there is only one repairperson, and she is not summoned " ++
  "until both machines are down. We find the proportion of up time. It " ++
  "should come out to about 0.45."

experiment :: Experiment
experiment =
  defaultExperiment {
    experimentSpecs = specs,
    experimentRunCount = 3,
    experimentDescription = description,
    experimentGenerators =
      [outputView defaultExperimentSpecsView,
       outputView $ defaultLastValueView {
         lastValueSeries = ["x"] },
       outputView $ defaultTimingStatsView {
         timingStatsSeries = ["x"] },
       outputView $ defaultFinalStatsView {
         finalStatsSeries = ["x"] },
       outputView $ defaultTableView {
         tableSeries = ["x"] }, 
       outputView $ defaultFinalTableView {
         finalTableSeries = ["x"] } ] }

upRate = 1.0 / 1.0       -- reciprocal of mean up time
repairRate = 1.0 / 0.5   -- reciprocal of mean repair time

exprnd :: Double -> IO Double
exprnd lambda =
  do x <- getStdRandom random
     return (- log x / lambda)
     
model :: Simulation ExperimentData
model =
  do -- number of machines currently up
     nUp <- newRef 2
     
     -- total up time for all machines
     totalUpTime <- newRef 0.0
     
     repairPerson <- newResource FCFS 1
     
     pid1 <- newProcessId
     pid2 <- newProcessId
     
     let machine :: ProcessId -> Process ()
         machine pid =
           do startUpTime <- liftDynamics time
              upTime <- liftIO $ exprnd upRate
              holdProcess upTime
              finishUpTime <- liftDynamics time
              liftEvent $ modifyRef totalUpTime 
                (+ (finishUpTime - startUpTime))
                
              liftEvent $ modifyRef nUp $ \a -> a - 1
              nUp' <- liftEvent $ readRef nUp
              if nUp' == 1
                then passivateProcess
                else liftEvent $
                     do n <- resourceCount repairPerson
                        when (n == 1) $ 
                          reactivateProcess pid
              
              requestResource repairPerson
              repairTime <- liftIO $ exprnd repairRate
              holdProcess repairTime
              liftEvent $ modifyRef nUp $ \a -> a + 1
              releaseResource repairPerson
              
              machine pid

     runProcessInStartTimeUsingId IncludingCurrentEvents
       pid1 (machine pid2)

     runProcessInStartTimeUsingId IncludingCurrentEvents
       pid2 (machine pid1)
     
     let result = 
           do x <- readRef totalUpTime
              y <- liftDynamics time
              return $ x / (2 * y)          
              
     experimentDataInStartTime
       [("x", seriesEntity "The proportion of up time" result),
        ("t", seriesEntity "Simulation time" time)]

main = runExperiment experiment model
