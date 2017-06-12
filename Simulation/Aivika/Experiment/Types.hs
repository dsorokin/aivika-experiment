
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
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

module Simulation.Aivika.Experiment.Types where

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent.ParallelIO.Local

import Data.Maybe
import Data.Monoid
import Data.Either

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika
import Simulation.Aivika.Trans.Exception

-- | It defines the simulation experiment with the specified rendering backend and its bound data.
data Experiment = 
  Experiment { experimentSpecs         :: Specs,
               -- ^ The simulation specs for the experiment.
               experimentTransform     :: ResultTransform,
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
defaultExperiment :: Experiment
defaultExperiment =
  Experiment { experimentSpecs         = Specs 0 10 0.01 RungeKutta4 SimpleGenerator,
               experimentTransform     = id,
               experimentLocalisation  = englishResultLocalisation,
               experimentRunCount      = 1,
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentNumCapabilities = getNumCapabilities }

-- | It allows rendering the simulation results in an arbitrary way.
class ExperimentRendering r where

  -- | Defines a context used when rendering the experiment.
  data ExperimentContext r :: *

  -- | Defines the experiment environment.
  type ExperimentEnvironment r :: *

  -- | Defines the experiment monad type.
  type ExperimentMonad r :: * -> *

  -- | Lift the experiment computation.
  liftExperiment :: r -> ExperimentMonad r a -> IO a

  -- | Prepare before rendering the experiment.
  prepareExperiment :: Experiment -> r -> ExperimentMonad r (ExperimentEnvironment r)

  -- | Render the experiment after the simulation is finished, for example,
  -- creating the @index.html@ file in the specified directory.
  renderExperiment :: Experiment -> r -> [ExperimentReporter r] -> ExperimentEnvironment r -> ExperimentMonad r ()

  -- | It is called when the experiment has been completed.
  onExperimentCompleted :: Experiment -> r -> ExperimentEnvironment r -> ExperimentMonad r () 

  -- | It is called when the experiment rendering has failed.
  onExperimentFailed :: Exception e => Experiment -> r -> ExperimentEnvironment r -> e -> ExperimentMonad r ()

-- | This is a generator of the reporter with the specified rendering backend.                     
data ExperimentGenerator r = 
  ExperimentGenerator { generateReporter :: Experiment -> r -> ExperimentEnvironment r -> ExperimentMonad r (ExperimentReporter r)
                        -- ^ Generate a reporter.
                      }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class ExperimentRendering r => ExperimentView v r where
  
  -- | Create a generator of the reporter.
  outputView :: v -> ExperimentGenerator r

-- | It describes the source simulation data used in the experiment.
data ExperimentData =
  ExperimentData { experimentResults :: Results,
                   -- ^ The simulation results used in the experiment.
                   experimentPredefinedSignals :: ResultPredefinedSignals
                   -- ^ The predefined signals provided by every model.
                 }

-- | Defines what creates the simulation reports by the specified renderer.
data ExperimentReporter r =
  ExperimentReporter { reporterInitialise :: ExperimentMonad r (),
                       -- ^ Initialise the reporting before 
                       -- the simulation runs are started.
                       reporterFinalise   :: ExperimentMonad r (),
                       -- ^ Finalise the reporting after
                       -- all simulation runs are finished.
                       reporterSimulate   :: ExperimentData -> Composite (),
                       -- ^ Start the simulation run in the start time.
                       reporterContext    :: ExperimentContext r
                       -- ^ Return a context used by the renderer.
                     }

-- | Run the simulation experiment sequentially. For example, 
-- it can be a Monte-Carlo simulation dependentent on the external
-- 'Parameter' values.
runExperiment :: (ExperimentRendering r,
                  Monad (ExperimentMonad r),
                  MonadIO (ExperimentMonad r),
                  MonadException (ExperimentMonad r))
                 => Experiment
                 -- ^ the simulation experiment to run
                 -> [ExperimentGenerator r]
                 -- ^ generators used for rendering
                 -> r
                 -- ^ the rendering backend
                 -> Simulation Results
                 -- ^ the simulation results received from the model
                 -> IO (Either SomeException ())
{-# INLINABLE runExperiment #-}
runExperiment e generators r simulation =
  runExperimentWithExecutor sequence_ e generators r simulation
  
-- | Run the simulation experiment in parallel. 
--
-- Make sure that you compile with @-threaded@ and supply @+RTS -N2 -RTS@ 
-- to the generated Haskell executable on dual core processor, 
-- or you won't get any parallelism. Generally, the mentioned 
-- @N@ parameter should correspond to the number of cores for 
-- your processor.
--
-- In case of need you might want to specify the number of
-- threads directly with help of 'experimentNumCapabilities',
-- although the real number of parallel threads can depend on many
-- factors.
runExperimentParallel :: (ExperimentRendering r,
                          Monad (ExperimentMonad r),
                          MonadIO (ExperimentMonad r),
                          MonadException (ExperimentMonad r))
                         => Experiment
                         -- ^ the simulation experiment to run
                         -> [ExperimentGenerator r]
                         -- ^ generators used for rendering
                         -> r
                         -- ^ the rendering backend
                         -> Simulation Results
                         -- ^ the simulation results received from the model
                         -> IO (Either SomeException ())
{-# INLINABLE runExperimentParallel #-}
runExperimentParallel e generators r simulation =
  do x <- runExperimentWithExecutor executor e generators r simulation
     return (x >> return ())
       where executor tasks =
               do n <- experimentNumCapabilities e
                  withPool n $ \pool ->
                    parallel_ pool tasks
                        
-- | Run the simulation experiment with the specified executor.
runExperimentWithExecutor :: (ExperimentRendering r,
                              Monad (ExperimentMonad r),
                              MonadIO (ExperimentMonad r),
                              MonadException (ExperimentMonad r))
                             => ([IO ()] -> IO a)
                             -- ^ an executor that allows parallelizing the simulation if required
                             -> Experiment
                             -- ^ the simulation experiment to run
                             -> [ExperimentGenerator r]
                             -- ^ generators used for rendering
                             -> r
                             -- ^ the rendering backend
                             -> Simulation Results
                             -- ^ the simulation results received from the model
                             -> IO (Either SomeException a)
{-# INLINABLE runExperimentWithExecutor #-}
runExperimentWithExecutor executor e generators r simulation =
  liftExperiment r $
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
     env <- prepareExperiment e r
     let c1 =
           do reporters <- mapM (\x -> generateReporter x e r env)
                           generators
              forM_ reporters reporterInitialise
              let simulate :: Simulation ()
                  simulate =
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
              a <- liftIO $
                executor $ runSimulations simulate specs runCount
              forM_ reporters reporterFinalise
              renderExperiment e r reporters env
              onExperimentCompleted e r env
              return (Right a)
         ch z@(SomeException e') =
           do onExperimentFailed e r env e'
              return (Left z)
     catchComp c1 ch
