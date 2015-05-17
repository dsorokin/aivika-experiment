
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
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
import Control.Monad.State
import Control.Concurrent.ParallelIO.Local

import Data.Maybe
import Data.Monoid

import System.Directory
import System.FilePath

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika
import Simulation.Aivika.Experiment.ExperimentWriter

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
               experimentDirectoryName :: ExperimentFilePath,
               -- ^ The directory in which the output results should be saved.
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
               experimentDirectoryName = UniqueFilePath "experiment",
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentNumCapabilities = getNumCapabilities }

-- | It allows rendering the simulation results in an arbitrary way.
class ExperimentRendering r where

  -- | Defines a context used when rendering the experiment.
  data ExperimentContext r :: *

  -- | Render the experiment after the simulation is finished, for example,
  -- creating the @index.html@ file in the specified directory.
  renderExperiment :: Experiment -> r -> [ExperimentReporter r] -> FilePath -> ExperimentWriter ()

-- | This is a generator of the reporter with the specified rendering backend.                     
data ExperimentGenerator r = 
  ExperimentGenerator { generateReporter :: Experiment -> r -> FilePath -> ExperimentWriter (ExperimentReporter r)
                        -- ^ Generate a reporter bound up with the specified directory.
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
  ExperimentReporter { reporterInitialise :: ExperimentWriter (),
                       -- ^ Initialise the reporting before 
                       -- the simulation runs are started.
                       reporterFinalise   :: ExperimentWriter (),
                       -- ^ Finalise the reporting after
                       -- all simulation runs are finished.
                       reporterSimulate   :: ExperimentData -> Event DisposableEvent,
                       -- ^ Start the simulation run in the start time
                       -- and return a finalizer that will be called 
                       -- in the stop time after the last signal is 
                       -- triggered and processed.
                       reporterContext    :: ExperimentContext r
                       -- ^ Return a context used by the renderer.
                     }

-- | Run the simulation experiment sequentially. For example, 
-- it can be a Monte-Carlo simulation dependentent on the external
-- 'Parameter' values.
runExperiment :: ExperimentRendering r
                 => Experiment
                 -- ^ the simulation experiment to run
                 -> [ExperimentGenerator r]
                 -- ^ generators used for rendering
                 -> r
                 -- ^ the rendering backend
                 -> Simulation Results
                 -- ^ the simulation results received from the model
                 -> IO ()
runExperiment = runExperimentWithExecutor sequence_
  
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
runExperimentParallel :: ExperimentRendering r
                         => Experiment
                         -- ^ the simulation experiment to run
                         -> [ExperimentGenerator r]
                         -- ^ generators used for rendering
                         -> r
                         -- ^ the rendering backend
                         -> Simulation Results
                         -- ^ the simulation results received from the model
                         -> IO ()
runExperimentParallel e = runExperimentWithExecutor executor e 
  where executor tasks =
          do n <- experimentNumCapabilities e
             withPool n $ \pool ->
               parallel_ pool tasks
                        
-- | Run the simulation experiment with the specified executor.
runExperimentWithExecutor :: ExperimentRendering r
                             => ([IO ()] -> IO ())
                             -- ^ an executor that allows parallelizing the simulation if required
                             -> Experiment
                             -- ^ the simulation experiment to run
                             -> [ExperimentGenerator r]
                             -- ^ generators used for rendering
                             -> r
                             -- ^ the rendering backend
                             -> Simulation Results
                             -- ^ the simulation results received from the model
                             -> IO ()
runExperimentWithExecutor executor e generators r simulation =
  runExperimentWriter $
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
         dirName    = experimentDirectoryName e
     path <- resolveFilePath "" dirName
     liftIO $ do
       when (experimentVerbose e) $
         do putStr "Updating directory " 
            putStrLn path
       createDirectoryIfMissing True path
     reporters <- mapM (\x -> generateReporter x e r path)
                  generators
     forM_ reporters reporterInitialise
     let simulate :: Simulation ()
         simulate =
           do signals <- newResultPredefinedSignals
              results <- simulation
              let d = ExperimentData { experimentResults = experimentTransform e results,
                                       experimentPredefinedSignals = signals }
              fs <- runDynamicsInStartTime $
                    runEventWith EarlierEvents $
                    forM reporters $ \reporter ->
                    reporterSimulate reporter d
              let m1 =
                    runEventInStopTime $
                    return ()
                  m2 =
                    runEventInStopTime $
                    disposeEvent $ mconcat fs
                  mh (SimulationException e') =
                    when (experimentVerbose e) $
                    liftIO $
                    do putStr "A simulation exception has been raised when running: " 
                       putStrLn $ show e'
              finallySimulation (catchSimulation m1 mh) m2
     liftIO $
       executor $ runSimulations simulate specs runCount
     forM_ reporters reporterFinalise
     renderExperiment e r reporters path
     return ()
