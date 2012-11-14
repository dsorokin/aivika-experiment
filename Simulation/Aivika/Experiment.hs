
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Experiment
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines the simulation experiments. They automate
-- the process of generating and analyzing the results. Moreover,
-- this module is open to extensions, allowing you to define
-- your own output views for the simulations results, for example,
-- such views that would allow saving the results in PDF or as
-- charts. To decrease the number of dependencies, such possible  
-- extenstions are not included in this package, although simple
-- views are provided.
--

module Simulation.Aivika.Experiment
       (Experiment(..),
        defaultExperiment,
        runExperiment,
        runExperimentParallel,
        ExperimentData(..),
        experimentDataInStartTime,
        experimentSeriesProviders,
        experimentMixedSignal,
        Series(..),
        SeriesEntity(..),
        SeriesProvider(..),
        View(..),
        Generator(..),
        Reporter(..),
        DirectoryName(..),
        resolveDirectoryName,
        FileName(..),
        resolveFileName) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent.ParallelIO.Local

import qualified Data.Map as M
import Data.Array
import Data.Maybe
import Data.Monoid
import Data.String.Utils (replace)

import qualified System.IO.UTF8 as UTF8
import System.Directory
import System.FilePath (combine)

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.Var
import Simulation.Aivika.Dynamics.UVar
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Parameter

import Simulation.Aivika.Experiment.HtmlWriter

-- | It defines the simulation experiment.
data Experiment = 
  Experiment { experimentSpecs         :: Specs,
               -- ^ The simulation specs for the experiment.
               experimentRunCount      :: Int,
               -- ^ How many simulation runs should be launched.
               experimentDirectoryName :: DirectoryName,
               -- ^ The directory in which the output results should be saved.
               experimentTitle         :: String,
               -- ^ The experiment title.
               experimentDescription   :: String,
               -- ^ The experiment description.
               experimentVerbose       :: Bool,
               -- ^ Whether the process of generating the results is verbose.
               experimentGenerators    :: [Generator], 
               -- ^ The experiment generators.
               experimentIndexHtml     :: Experiment -> [Reporter] -> FilePath -> IO ()
               -- ^ Create the @index.html@ file after the simulation is finished
               -- in the specified directory. 
             }

-- | The default experiment.
defaultExperiment :: Experiment
defaultExperiment =
  Experiment { experimentSpecs         = Specs 0 10 0.01 RungeKutta4,
               experimentRunCount      = 1,
               experimentDirectoryName = UniqueDirectoryName "experiment",
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentGenerators    = [], 
               experimentIndexHtml     = createIndexHtml }

-- | This is a generator of the reporter.                     
data Generator = 
  Generator { generateReporter :: Experiment -> FilePath -> IO Reporter 
              -- ^ Generate a reporter for the specified directory,
              -- where the @index.html@ file will be saved for the 
              -- current simulation experiment.
            }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class View v where
  
  -- | Create a generator of the reporter.
  outputView :: v -> Generator
  
-- | Represents the series. It is usually something, or
-- an array of something, or a list of such values which
-- can be simulated.
class Series s where
  
  -- | Return the simulatable entity with the specified name
  -- for the given series.
  seriesEntity :: String -> s -> SeriesEntity
  
-- | Defines the simulatable entity.
data SeriesEntity =
  SeriesEntity { seriesProviders :: [SeriesProvider]
                 -- ^ Return the providers for the entity.
               }
  
-- | This is provider of the simulatable data.
data SeriesProvider =
  SeriesProvider { providerName :: String,
                   -- ^ Return the name.
                   providerToDouble :: Maybe (Dynamics Double),
                   -- ^ Try to return the data as double values.
                   providerToInt :: Maybe (Dynamics Int),
                   -- ^ Try to return the data as integers.
                   providerToString :: Maybe (Dynamics String),
                   -- ^ Try to return the data as strings.
                   providerSignal :: Maybe (Signal ())
                   -- ^ Try to get a signal for the data, which
                   -- is actual for the 'Ref' references and 
                   -- the 'Var' variables. You should not provide
                   -- such a signal if the data are calculated
                   -- only in the integration time points, which
                   -- is true for the integrals, for example.
                 }

-- | It describes the source simulation data used in the experiment.
data ExperimentData =
  ExperimentData { experimentQueue :: EventQueue,
                   -- ^ Return the event queue.
                   experimentSignalInIntegTimes :: Signal Double,
                   -- ^ The signal triggered in the integration time points.
                   experimentSignalInStartTime :: Signal Double,
                   -- ^ The signal triggered in the start time.
                   experimentSignalInStopTime :: Signal Double,
                   -- ^ The signal triggered in the stop time.
                   experimentSeries :: M.Map String SeriesEntity
                   -- ^ The simulation entitities with labels as keys.
                 }

-- | Prepare data for the simulation experiment in start time from the series 
-- with the specified labels.
experimentDataInStartTime :: EventQueue -> [(String, SeriesEntity)] -> Simulation ExperimentData
experimentDataInStartTime q m = runDynamicsInStartTime d where
  d = do signalInIntegTimes <- newSignalInIntegTimes q
         signalInStartTime  <- newSignalInStartTime q
         signalInStopTime   <- newSignalInStopTime q
         let series = M.fromList m
         return ExperimentData { experimentQueue              = q,
                                 experimentSignalInIntegTimes = signalInIntegTimes,
                                 experimentSignalInStartTime  = signalInStartTime,
                                 experimentSignalInStopTime   = signalInStopTime,
                                 experimentSeries             = series }

-- | Get a mixed signal for the specified providers based on 
-- the experimental data. This signal is triggered when 
-- the provided signals are triggered. The mixed signal is 
-- also triggered in the integration time points if there is 
-- at least one provider without signal.
experimentMixedSignal :: ExperimentData -> [SeriesProvider] -> Signal ()
experimentMixedSignal expdata providers =
  let xs0 = map providerSignal providers
      xs1 = filter isJust xs0
      xs2 = filter isNothing xs0
      signal1 = mconcat $ map fromJust xs1
      signal2 = if null xs2 
                then signal3 <> signal4
                else signal5
      signal3 = void $ experimentSignalInStartTime expdata
      signal4 = void $ experimentSignalInStopTime expdata
      signal5 = void $ experimentSignalInIntegTimes expdata
  in signal1 <> signal2

-- | Return the 'SeriesProvider' values from the experiment data by the specified labels.
experimentSeriesProviders :: ExperimentData -> [String] -> [SeriesProvider]
experimentSeriesProviders expdata labels =
  join $ flip map labels $ \label ->
  case M.lookup label (experimentSeries expdata) of
    Nothing -> 
      error $ 
      "There is no series with label " ++ label ++ 
      ": experimentSeriesProviders"
    Just entity -> 
      seriesProviders entity

-- | Defines what creates the simulation reports.
data Reporter =
  Reporter { reporterInitialise :: IO (),
             -- ^ Initialise the reporting before 
             -- the simulation runs are started.
             reporterFinalise   :: IO (),
             -- ^ Finalise the reporting after
             -- all simulation runs are finished.
             reporterSimulate   :: ExperimentData -> 
                                   Dynamics (Dynamics ()),
             -- ^ Start the simulation run in the start time
             -- and return a finalizer that will be called 
             -- in the stop time after the last signal is 
             -- triggered and processed.
             reporterTOCHtml :: Int -> HtmlWriter (),
             -- ^ Return a TOC (Table of Contents) item for 
             -- the HTML index file after the finalisation 
             -- function is called, i.e. in the very end. 
             -- The agument specifies the ordered number of 
             -- the item.
             --
             -- You should wrap your HTML in 'writeHtmlListItem'.
             reporterHtml :: Int -> HtmlWriter ()
             -- ^ Return an HTML code for the index file
             -- after the finalisation function is called,
             -- i.e. in the very end. The agument specifies
             -- the ordered number of the item.
           }

-- | Run the simulation experiment sequentially. For example, 
-- it can be a Monte-Carlo simulation dependentent on the external
-- 'Parameter' values.
runExperiment :: Experiment -> Simulation ExperimentData -> IO ()
runExperiment = runExperimentWithExecutor sequence_
  
-- | Run the simulation experiment parallelly. 
--
-- Make sure that you compile with @-threaded@ and supply @+RTS -N2 -RTS@ 
-- to the generated Haskell executable on dual core processor, 
-- or you won't get any parallelism. Generally, the mentioned 
-- @N@ parameter should correspond to the number of cores for 
-- your processor.
runExperimentParallel :: Experiment -> Simulation ExperimentData -> IO ()
runExperimentParallel = runExperimentWithExecutor executor 
  where executor tasks =
          do n <- getNumCapabilities
             withPool n $ \pool ->
               parallel_ pool tasks
                        
-- | Run the simulation experiment with the specified executor.
runExperimentWithExecutor :: ([IO ()] -> IO ()) ->
                             Experiment -> 
                             Simulation ExperimentData -> IO ()
runExperimentWithExecutor executor e simulation = 
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
         dirName    = experimentDirectoryName e
         generators = experimentGenerators e
     path <- resolveDirectoryName Nothing dirName M.empty
     when (experimentVerbose e) $
       do putStr "Using directory " 
          putStrLn path
     createDirectoryIfMissing True path
     reporters <- mapM (\x -> generateReporter x e path)
                  generators
     forM_ reporters reporterInitialise
     let simulate :: Simulation ()
         simulate =
           do d  <- simulation
              fs <- runDynamicsInStartTime $
                    forM reporters $ \reporter ->
                    reporterSimulate reporter d
              runDynamicsInStopTime $
                do updateSignal $ 
                     experimentMixedSignal d $
                     join $ map seriesProviders $
                     M.elems $ experimentSeries d
                   sequence_ fs
     executor $ runSimulations simulate specs runCount
     forM_ reporters reporterFinalise
     experimentIndexHtml e e reporters path
     return ()
     
-- | Create an index HTML file.     
createIndexHtml :: Experiment -> [Reporter] -> FilePath -> IO ()
createIndexHtml e reporters path = 
  do let html :: HtmlWriter ()
         html = 
           writeHtmlDocumentWithTitle (experimentTitle e) $
             do writeHtmlList $
                  forM_ (zip [1..] reporters) $ \(i, reporter) -> 
                  reporterTOCHtml reporter i
                writeHtmlBreak
                unless (null $ experimentDescription e) $
                  writeHtmlParagraph $
                  writeHtmlText $ experimentDescription e
                forM_ (zip [1..] reporters) $ \(i, reporter) ->
                  reporterHtml reporter i
         file = combine path "index.html"
     ((), contents) <- runHtmlWriter html id
     UTF8.writeFile file (contents [])
     when (experimentVerbose e) $
       do putStr "Generated file "
          putStrLn file

-- | Specifies the directory name, unique or writable.
data DirectoryName = WritableDirectoryName String
                     -- ^ The directory which is overwritten in 
                     -- case if it existed before.
                   | UniqueDirectoryName String
                     -- ^ The directory which is always unique,
                     -- when a prefix is added to the name
                     -- in case of need.
                
-- | Specifies the file name, unique or writable.
data FileName = WritableFileName String String
                -- ^ The file which is overwritten in 
                -- case if it existed before. The first
                -- field defines a name or its prototype.
                -- The second field is the file extension.
              | UniqueFileName String String
                -- ^ The file which is always unique,
                -- when a prefix is added to the name
                -- in case of need. The first field
                -- defines a name or its prototype.
                -- The second field is the file exension.
                
-- | Resolve the directory name relative to the passed in directory 
-- as the first argument, replacing the specified strings according the map. 
resolveDirectoryName :: Maybe FilePath -> DirectoryName -> M.Map String String -> IO String
resolveDirectoryName dir (WritableDirectoryName name) map = 
  return $ replaceName (combineName dir name) map
resolveDirectoryName dir (UniqueDirectoryName name) map =
  let x = replaceName name map
      loop y i =
        do let n = combineName dir y
           f1 <- doesFileExist n
           f2 <- doesDirectoryExist n
           if f1 || f2
             then loop (x ++ "(" ++ show i ++ ")") (i + 1)
             else return n
  in loop x 2
     
-- | Resolve the file name relative to the passed in directory 
-- as the first argument, replacing the specified strings according the map. 
resolveFileName :: Maybe FilePath -> FileName -> M.Map String String -> IO String
resolveFileName dir (WritableFileName name ext) map = 
  return $ replaceName (combineName dir name ++ ext) map
resolveFileName dir (UniqueFileName name ext) map =
  let x = replaceName name map
      loop y i =
        do let n = combineName dir y ++ ext
           f1 <- doesFileExist n
           f2 <- doesDirectoryExist n
           if f1 || f2
             then loop (x ++ "(" ++ show i ++ ")") (i + 1)
             else return n
  in loop x 2
     
-- | Replace the name according the specified table.
replaceName :: String -> M.Map String String -> String     
replaceName name map = name' where
  ((), name') = flip runState name $
                forM_ (M.assocs map) $ \(k, v) ->
                do a <- get
                   put $ replace k v a
                   
-- | Combine the file name with the directory name.
combineName :: Maybe String -> String -> String                   
combineName dir name =
  case dir of
    Nothing  -> name
    Just dir -> combine dir name

instance Series (Simulation Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ liftSimulation s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ liftSimulation $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Simulation Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ liftSimulation $ fmap fromIntegral s,
                                        providerToInt    = Just $ liftSimulation s,
                                        providerToString = Just $ liftSimulation $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Simulation String) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ liftSimulation s,
                                        providerSignal   = Nothing }] }

instance Series (Dynamics Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Dynamics Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap fromIntegral s,
                                        providerToInt    = Just s,
                                        providerToString = Just $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Dynamics String) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just s,
                                        providerSignal   = Nothing }] }

instance Series (Ref Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readRef s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readRef s),
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Ref Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap fromIntegral (readRef s),
                                        providerToInt    = Just $ readRef s,
                                        providerToString = Just $ fmap show (readRef s),
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Ref String) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ readRef s,
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Var Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readVar s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readVar s),
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (Var Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap fromIntegral (readVar s),
                                        providerToInt    = Just $ readVar s,
                                        providerToString = Just $ fmap show (readVar s),
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (Var String) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ readVar s,
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (UVar Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readUVar s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }

instance Series (UVar Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap fromIntegral (readUVar s),
                                        providerToInt    = Just $ readUVar s,
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }
    
instance Series s => Series [s] where
  
  seriesEntity name s = 
    SeriesEntity { seriesProviders = 
                      join $ forM (zip [1..] s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity name' s }
    
instance (Show i, Ix i, Series s) => Series (Array i s) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      join $ forM (assocs s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity name' s }
