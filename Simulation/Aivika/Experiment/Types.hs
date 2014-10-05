
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes, FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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

module Simulation.Aivika.Experiment.Types
       (-- * General Definitions
        Experiment(..),
        ExperimentRendering(..),
        defaultExperiment,
        runExperiment,
        runExperimentParallel,
        ExperimentData(..),
        ExperimentView(..),
        ExperimentGenerator(..),
        ExperimentReporter(..),
        ExperimentFilePath(..),
        resolveFilePath,
        expandFilePath,
        mapFilePath,
        -- * Web Page Rendering
        WebPageRendering(..),
        WebPageRenderer(..),
        WebPageWriter(..)) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent.ParallelIO.Local

import qualified Data.Map as M

import Data.Ix
import Data.Maybe
import Data.Monoid

import qualified System.IO.UTF8 as UTF8
import System.Directory
import System.FilePath

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

-- | It defines the simulation experiment with the specified rendering backend and its bound data.
data Experiment r a = 
  Experiment { experimentSpecs         :: Specs,
               -- ^ The simulation specs for the experiment.
               experimentTransform     :: ResultTransform,
               -- ^ How the results must be transformed before rendering.
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
               experimentGenerators    :: [ExperimentGenerator r a], 
               -- ^ The experiment generators.
               experimentNumCapabilities :: IO Int
               -- ^ The number of threads used for the Monte-Carlo simulation
               -- if the executable was compiled with the support of multi-threading.
             }

-- | The default experiment.
defaultExperiment :: Experiment r a
defaultExperiment =
  Experiment { experimentSpecs         = Specs 0 10 0.01 RungeKutta4 SimpleGenerator,
               experimentTransform     = id,
               experimentRunCount      = 1,
               experimentDirectoryName = UniqueFilePath "experiment",
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentGenerators    = [], 
               experimentNumCapabilities = getNumCapabilities }

-- | It allows rendering the simulation results in an arbitrary way.
class ExperimentRendering r a | r -> a where

  -- | Render the experiment after the simulation is finished, for example,
  -- creating the @index.html@ file in the specified directory.
  renderExperiment :: Experiment r a -> r -> [ExperimentReporter r a] -> FilePath -> IO ()

-- | This is a generator of the reporter with the specified rendering backend.                     
data ExperimentGenerator r a = 
  ExperimentGenerator { generateReporter :: Experiment r a -> r -> FilePath -> IO (ExperimentReporter r a)
                        -- ^ Generate a reporter bound up with the specified directory.
                      }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class ExperimentRendering r a => ExperimentView v r a | r -> a where
  
  -- | Create a generator of the reporter.
  outputView :: v -> ExperimentGenerator r a

-- | It describes the source simulation data used in the experiment.
data ExperimentData =
  ExperimentData { experimentResults :: Results,
                   -- ^ The simulation results used in the experiment.
                   experimentPredefinedSignals :: ResultPredefinedSignals
                   -- ^ The predefined signals provided by every model.
                 }

-- | Defines what creates the simulation reports by the specified renderer.
data ExperimentReporter r a =
  ExperimentReporter { reporterInitialise :: IO (),
                       -- ^ Initialise the reporting before 
                       -- the simulation runs are started.
                       reporterFinalise   :: IO (),
                       -- ^ Finalise the reporting after
                       -- all simulation runs are finished.
                       reporterSimulate   :: ExperimentData -> Event DisposableEvent,
                       -- ^ Start the simulation run in the start time
                       -- and return a finalizer that will be called 
                       -- in the stop time after the last signal is 
                       -- triggered and processed.
                       reporterRequest    :: a
                       -- ^ Return data requested by the renderer.
                     }

-- | Run the simulation experiment sequentially. For example, 
-- it can be a Monte-Carlo simulation dependentent on the external
-- 'Parameter' values.
runExperiment :: ExperimentRendering r a
                 => Experiment r a
                 -- ^ the simulation experiment to run
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
runExperimentParallel :: ExperimentRendering r a
                         => Experiment r a
                         -- ^ the simulation experiment to run
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
runExperimentWithExecutor :: ExperimentRendering r a 
                             => ([IO ()] -> IO ())
                             -- ^ an executor that allows parallelizing the simulation if required
                             -> Experiment r a
                             -- ^ the simulation experiment to run
                             -> r
                             -- ^ the rendering backend
                             -> Simulation Results
                             -- ^ the simulation results received from the model
                             -> IO ()
runExperimentWithExecutor executor e r simulation = 
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
         dirName    = experimentDirectoryName e
         generators = experimentGenerators e
     path <- resolveFilePath "" dirName
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
              runEventInStopTime $
                disposeEvent $ mconcat fs
     executor $ runSimulations simulate specs runCount
     forM_ reporters reporterFinalise
     renderExperiment e r reporters path
     return ()

-- | It defines the web page renderer for simulation 'Experiment'. 
data WebPageRenderer = WebPageRenderer

-- | It replies to the requests made by the web page renderer.
data WebPageWriter =
  WebPageWriter { reporterWriteTOCHtml :: Int -> HtmlWriter (),
                  -- ^ Return a TOC (Table of Contents) item for 
                  -- the HTML index file after the finalisation 
                  -- function is called, i.e. in the very end. 
                  -- The agument specifies the ordered number of 
                  -- the item.
                  --
                  -- You should wrap your HTML in 'writeHtmlListItem'.
                  reporterWriteHtml :: Int -> HtmlWriter ()
                  -- ^ Return an HTML code for the index file
                  -- after the finalisation function is called,
                  -- i.e. in the very end. The agument specifies
                  -- the ordered number of the item.
                }

-- | A subclass of renderers that know how to save the @index.html@ file
-- when rendering the simulation experiment.
class ExperimentRendering r WebPageWriter => WebPageRendering r

instance WebPageRendering WebPageRenderer

instance ExperimentRendering WebPageRenderer WebPageWriter where

  renderExperiment e r reporters path = 
    do let html :: HtmlWriter ()
           html = 
             writeHtmlDocumentWithTitle (experimentTitle e) $
             do writeHtmlList $
                  forM_ (zip [1..] reporters) $ \(i, reporter) -> 
                  reporterWriteTOCHtml (reporterRequest reporter) i
                writeHtmlBreak
                unless (null $ experimentDescription e) $
                  writeHtmlParagraph $
                  writeHtmlText $ experimentDescription e
                forM_ (zip [1..] reporters) $ \(i, reporter) ->
                  reporterWriteHtml (reporterRequest reporter) i
           file = combine path "index.html"
       ((), contents) <- runHtmlWriter html id
       UTF8.writeFile file (contents [])
       when (experimentVerbose e) $
         do putStr "Generated file "
            putStrLn file
  
-- | Specifies the file name, unique or writable, which can be appended with extension if required.
data ExperimentFilePath = WritableFilePath FilePath
                          -- ^ The file which is overwritten in 
                          -- case if it existed before.
                        | UniqueFilePath FilePath
                          -- ^ The file which is always unique,
                          -- when an automatically generated suffix
                          -- is added to the name in case of need.
                
-- | Resolve the file path relative to the specified directory passed in the first argument
-- and taking into account a possible requirement to have an unique file name.
resolveFilePath :: FilePath -> ExperimentFilePath -> IO FilePath
resolveFilePath dir (WritableFilePath path) =
  return $ dir </> path
resolveFilePath dir (UniqueFilePath path)   =
  let (name, ext) = splitExtension path
      loop y i =
        do let n = dir </> addExtension y ext
           f1 <- doesFileExist n
           f2 <- doesDirectoryExist n
           if f1 || f2
             then loop (name ++ "(" ++ show i ++ ")") (i + 1)
             else return n
  in loop name 2

-- | Expand the file path using the specified table of substitutions.
expandFilePath :: ExperimentFilePath -> M.Map String String -> ExperimentFilePath
expandFilePath (WritableFilePath path) map = WritableFilePath (expandTemplates path map)
expandFilePath (UniqueFilePath path) map = UniqueFilePath (expandTemplates path map)

-- | Expand the string templates using the specified table of substitutions.
expandTemplates :: String -> M.Map String String -> String     
expandTemplates name map = name' where
  ((), name') = flip runState name $
                forM_ (M.assocs map) $ \(k, v) ->
                do a <- get
                   put $ replace k v a

-- | Transform the file path using the specified function.
mapFilePath :: (FilePath -> FilePath) -> ExperimentFilePath -> ExperimentFilePath
mapFilePath f (WritableFilePath path) = WritableFilePath (f path)
mapFilePath f (UniqueFilePath path) = UniqueFilePath (f path) 
