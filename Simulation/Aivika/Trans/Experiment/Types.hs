
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Experiment.Types
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines the simulation experiments. They automate
-- the process of generating and analyzing the results. Moreover,
-- this module is open to extensions, allowing you to define
-- your own output views for the simulation results, for example,
-- such views that would allow saving the results in PDF or as
-- charts. To decrease the number of dependencies, such possible  
-- extenstions are not included in this package, although simple
-- views are provided.
--

module Simulation.Aivika.Trans.Experiment.Types where

import Control.Monad
import Control.Exception

import Data.Maybe
import Data.Monoid
import Data.Either

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika.Trans

-- | It defines the simulation experiment with the specified rendering backend and its bound data.
data Experiment m = 
  Experiment { experimentSpecs         :: Specs m,
               -- ^ The simulation specs for the experiment.
               experimentTransform     :: ResultTransform m,
               -- ^ How the results must be transformed before rendering.
               experimentLocalisation  :: ResultLocalisation,
               -- ^ Specifies a localisation applied when rendering the experiment.
               experimentRunCount      :: Int,
               -- ^ How many simulation runs should be launched.
               experimentTitle         :: String,
               -- ^ The experiment title.
               experimentDescription   :: String,
               -- ^ The experiment description.
               experimentVerbose       :: Bool,
               -- ^ Whether the process of generating the results is verbose.
               experimentNumCapabilities :: IO Int
               -- ^ The number of threads used for the Monte-Carlo simulation
               -- if the executable was compiled with the support of multi-threading.
             }

-- | The default experiment.
defaultExperiment :: Experiment m
defaultExperiment =
  Experiment { experimentSpecs         = Specs 0 10 0.01 RungeKutta4 SimpleGenerator,
               experimentTransform     = id,
               experimentLocalisation  = englishResultLocalisation,
               experimentRunCount      = 1,
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentNumCapabilities = getNumCapabilities }

-- | Allows specifying the experiment monad.
class ExperimentMonadProviding r (m :: * -> *) where

  -- | Defines the experiment monad type.
  type ExperimentMonad r m :: * -> *

-- | Defines the experiment computation that tries to perform the calculation. 
type ExperimentMonadTry r m a = ExperimentMonad r m (Either SomeException a)

-- | It allows rendering the simulation results in an arbitrary way.
class ExperimentMonadProviding r m => ExperimentRendering r m where

  -- | Defines a context used when rendering the experiment.
  data ExperimentContext r m :: *

  -- | Defines the experiment environment.
  type ExperimentEnvironment r m :: *

  -- | Prepare before rendering the experiment.
  prepareExperiment :: Experiment m -> r -> ExperimentMonad r m (ExperimentEnvironment r m)

  -- | Render the experiment after the simulation is finished, for example,
  -- creating the @index.html@ file in the specified directory.
  renderExperiment :: Experiment m -> r -> [ExperimentReporter r m] -> ExperimentEnvironment r m -> ExperimentMonad r m ()

  -- | It is called when the experiment has been completed.
  onExperimentCompleted :: Experiment m -> r -> ExperimentEnvironment r m -> ExperimentMonad r m () 

  -- | It is called when the experiment rendering has failed.
  onExperimentFailed :: Exception e => Experiment m -> r -> ExperimentEnvironment r m -> e -> ExperimentMonad r m ()

-- | This is a generator of the reporter with the specified rendering backend.                     
data ExperimentGenerator r m = 
  ExperimentGenerator { generateReporter :: Experiment m -> r -> ExperimentEnvironment r m -> ExperimentMonad r m (ExperimentReporter r m)
                        -- ^ Generate a reporter.
                      }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class ExperimentRendering r m => ExperimentView v r m where
  
  -- | Create a generator of the reporter.
  outputView :: v m -> ExperimentGenerator r m

-- | It describes the source simulation data used in the experiment.
data ExperimentData m =
  ExperimentData { experimentResults :: Results m,
                   -- ^ The simulation results used in the experiment.
                   experimentPredefinedSignals :: ResultPredefinedSignals m
                   -- ^ The predefined signals provided by every model.
                 }

-- | Defines what creates the simulation reports by the specified renderer.
data ExperimentReporter r m =
  ExperimentReporter { reporterInitialise :: ExperimentMonad r m (),
                       -- ^ Initialise the reporting before 
                       -- the simulation runs are started.
                       reporterFinalise   :: ExperimentMonad r m (),
                       -- ^ Finalise the reporting after
                       -- all simulation runs are finished.
                       reporterSimulate   :: ExperimentData m -> Composite m (),
                       -- ^ Start the simulation run in the start time.
                       reporterContext    :: ExperimentContext r m
                       -- ^ Return a context used by the renderer.
                     }

-- | Run the simulation experiment sequentially.
runExperiment_ :: (MonadDES m,
                   ExperimentRendering r m,
                   Monad (ExperimentMonad r m),
                   MonadException (ExperimentMonad r m))
                  => (m () -> ExperimentMonad r m a)
                  -- ^ the function that actually starts the simulation run
                  -> Experiment m
                  -- ^ the simulation experiment to run
                  -> [ExperimentGenerator r m]
                  -- ^ generators used for rendering
                  -> r
                  -- ^ the rendering backend
                  -> Simulation m (Results m)
                  -- ^ the simulation results received from the model
                  -> ExperimentMonadTry r m ()
{-# INLINABLE runExperiment_ #-}
runExperiment_ executor0 e generators r simulation =
  do x <- runExperimentWithExecutor executor e generators r simulation
     return (x >> return ())
       where executor = sequence_ . map executor0

-- | Run the simulation experiment sequentially.
runExperiment :: (MonadDES m,
                  ExperimentRendering r m,
                  Monad (ExperimentMonad r m),
                  MonadException (ExperimentMonad r m))
                 => (m () -> ExperimentMonad r m a)
                 -- ^ the function that actually starts the simulation run
                 -> Experiment m
                 -- ^ the simulation experiment to run
                 -> [ExperimentGenerator r m]
                 -- ^ generators used for rendering
                 -> r
                 -- ^ the rendering backend
                 -> Simulation m (Results m)
                 -- ^ the simulation results received from the model
                 -> ExperimentMonadTry r m [a]
{-# INLINABLE runExperiment #-}
runExperiment executor0 = runExperimentWithExecutor executor
  where executor = sequence . map executor0

-- | Run the simulation experiment with the specified executor.
runExperimentWithExecutor :: (MonadDES m,
                              ExperimentRendering r m,
                              Monad (ExperimentMonad r m),
                              MonadException (ExperimentMonad r m))
                             => ([m ()] -> ExperimentMonad r m a)
                             -- ^ an executor that allows parallelizing the simulation if required
                             -> Experiment m
                             -- ^ the simulation experiment to run
                             -> [ExperimentGenerator r m]
                             -- ^ generators used for rendering
                             -> r
                             -- ^ the rendering backend
                             -> Simulation m (Results m)
                             -- ^ the simulation results received from the model
                             -> ExperimentMonadTry r m a
{-# INLINABLE runExperimentWithExecutor #-}
runExperimentWithExecutor executor e generators r simulation =
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
     env <- prepareExperiment e r
     let c1 =
           do reporters <- mapM (\x -> generateReporter x e r env)
                           generators
              forM_ reporters reporterInitialise
              let simulate =
                    do signals <- newResultPredefinedSignals
                       results <- simulation
                       let d = ExperimentData { experimentResults = experimentTransform e results,
                                                experimentPredefinedSignals = signals }
                       ((), fs) <- runDynamicsInStartTime $
                                   runEventWith EarlierEvents $
                                   flip runComposite mempty $
                                   forM_ reporters $ \reporter ->
                                   reporterSimulate reporter d
                       let m1 =
                             runEventInStopTime $
                             return ()
                           m2 =
                             runEventInStopTime $
                             disposeEvent fs
                           mh (SimulationAbort e') =
                             return ()
                       finallySimulation (catchSimulation m1 mh) m2
              a <- executor $
                   runSimulations simulate specs runCount
              forM_ reporters reporterFinalise
              renderExperiment e r reporters env
              onExperimentCompleted e r env
              return (Right a)
         ch z@(SomeException e') =
           do onExperimentFailed e r env e'
              return (Left z)
     catchComp c1 ch

-- | Run the simulation experiment by the specified run index in series.
runExperimentByIndex :: (MonadDES m,
                         ExperimentRendering r m,
                         Monad (ExperimentMonad r m),
                         MonadException (ExperimentMonad r m))
                        => (m () -> ExperimentMonad r m a)
                        -- ^ the function that actually starts the simulation run
                        -> Experiment m
                        -- ^ the simulation experiment to run
                        -> [ExperimentGenerator r m]
                        -- ^ generators used for rendering
                        -> r
                        -- ^ the rendering backend
                        -> Simulation m (Results m)
                        -- ^ the simulation results received from the model
                        -> Int
                        -- ^ the index of the current run (started from 1)
                        -> ExperimentMonadTry r m a
{-# INLINABLE runExperimentByIndex #-}
runExperimentByIndex executor e generators r simulation runIndex =
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
     env <- prepareExperiment e r
     let c1 =
           do reporters <- mapM (\x -> generateReporter x e r env)
                           generators
              forM_ reporters reporterInitialise
              let simulate =
                    do signals <- newResultPredefinedSignals
                       results <- simulation
                       let d = ExperimentData { experimentResults = experimentTransform e results,
                                                experimentPredefinedSignals = signals }
                       ((), fs) <- runDynamicsInStartTime $
                                   runEventWith EarlierEvents $
                                   flip runComposite mempty $
                                   forM_ reporters $ \reporter ->
                                   reporterSimulate reporter d
                       let m1 =
                             runEventInStopTime $
                             return ()
                           m2 =
                             runEventInStopTime $
                             disposeEvent fs
                           mh (SimulationAbort e') =
                             return ()
                       finallySimulation (catchSimulation m1 mh) m2
              a <- executor $
                   runSimulationByIndex simulate specs runCount runIndex
              forM_ reporters reporterFinalise
              renderExperiment e r reporters env
              onExperimentCompleted e r env
              return (Right a)
         ch z@(SomeException e') =
           do onExperimentFailed e r env e'
              return (Left z)
     catchComp c1 ch

-- | Run the simulation experiment by the specified run index in series.
runExperimentByIndex_ :: (MonadDES m,
                          ExperimentRendering r m,
                          Monad (ExperimentMonad r m),
                          MonadException (ExperimentMonad r m))
                         => (m () -> ExperimentMonad r m a)
                         -- ^ the function that actually starts the simulation run
                         -> Experiment m
                         -- ^ the simulation experiment to run
                         -> [ExperimentGenerator r m]
                         -- ^ generators used for rendering
                         -> r
                         -- ^ the rendering backend
                         -> Simulation m (Results m)
                         -- ^ the simulation results received from the model
                         -> Int
                         -- ^ the index of the current run (started from 1)
                         -> ExperimentMonadTry r m ()
{-# INLINABLE runExperimentByIndex_ #-}
runExperimentByIndex_ executor e generators r simulation runIndex =
  do x <- runExperimentByIndex executor e generators r simulation runIndex
     return (x >> return ())

-- | Run the simulation experiment by the specified run index in series
-- returning the continuation of the actual computation.
runExperimentContByIndex :: (MonadDES m,
                             ExperimentRendering r m,
                             Monad (ExperimentMonad r m),
                             MonadException (ExperimentMonad r m))
                            => (m () -> ExperimentMonad r m (a, ExperimentMonad r m b))
                            -- ^ the function that actually starts the simulation run
                            -- and returns the corresponding continuation
                            -> Experiment m
                            -- ^ the simulation experiment to run
                            -> [ExperimentGenerator r m]
                            -- ^ generators used for rendering
                            -> r
                            -- ^ the rendering backend
                            -> Simulation m (Results m)
                            -- ^ the simulation results received from the model
                            -> Int
                            -- ^ the index of the current run (started from 1)
                            -> ExperimentMonadTry r m (a, ExperimentMonadTry r m b)
{-# INLINABLE runExperimentContByIndex #-}
runExperimentContByIndex executor e generators r simulation runIndex =
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
     env <- prepareExperiment e r
     let c1 =
           do reporters <- mapM (\x -> generateReporter x e r env)
                           generators
              forM_ reporters reporterInitialise
              let simulate =
                    do signals <- newResultPredefinedSignals
                       results <- simulation
                       let d = ExperimentData { experimentResults = experimentTransform e results,
                                                experimentPredefinedSignals = signals }
                       ((), fs) <- runDynamicsInStartTime $
                                   runEventWith EarlierEvents $
                                   flip runComposite mempty $
                                   forM_ reporters $ \reporter ->
                                   reporterSimulate reporter d
                       let m1 =
                             runEventInStopTime $
                             return ()
                           m2 =
                             runEventInStopTime $
                             disposeEvent fs
                           mh (SimulationAbort e') =
                             return ()
                       finallySimulation (catchSimulation m1 mh) m2
              (a, m) <- executor $
                        runSimulationByIndex simulate specs runCount runIndex
              let m2 =
                    do b <- m
                       forM_ reporters reporterFinalise
                       renderExperiment e r reporters env
                       onExperimentCompleted e r env
                       return (Right b)
                  mh z@(SomeException e') =
                    do onExperimentFailed e r env e'
                       return (Left z)
              return (Right (a, catchComp m2 mh))
         ch z@(SomeException e') =
           do onExperimentFailed e r env e'
              return (Left z)
     catchComp c1 ch

-- | Run the simulation experiment by the specified run index in series
-- returning the continuation of the actual computation.
runExperimentContByIndex_ :: (MonadDES m,
                              ExperimentRendering r m,
                              Monad (ExperimentMonad r m),
                              MonadException (ExperimentMonad r m))
                             => (m () -> ExperimentMonad r m (a, ExperimentMonad r m b))
                             -- ^ the function that actually starts the simulation run
                             -- and returns the corresponding continuation
                             -> Experiment m
                             -- ^ the simulation experiment to run
                             -> [ExperimentGenerator r m]
                             -- ^ generators used for rendering
                             -> r
                             -- ^ the rendering backend
                             -> Simulation m (Results m)
                             -- ^ the simulation results received from the model
                             -> Int
                             -- ^ the index of the current run (started from 1)
                             -> ExperimentMonadTry r m (a, ExperimentMonadTry r m ())
{-# INLINABLE runExperimentContByIndex_ #-}
runExperimentContByIndex_ executor e generators r simulation runIndex =
  do x <- runExperimentContByIndex executor e generators r simulation runIndex
     case x of
       Left e -> return (Left e)
       Right (a, cont) -> return $ Right (a, cont >> return (Right ()))
