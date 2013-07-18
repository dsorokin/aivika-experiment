
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.Experiment
-- Copyright  : Copyright (c) 2012-2013, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
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
        SeriesContainer(..),
        SeriesEntity(..),
        SeriesProvider(..),
        SeriesListWithSubscript,
        SeriesArrayWithSubscript,
        SeriesVectorWithSubscript,
        seriesListWithSubscript,
        seriesArrayWithSubscript,
        seriesVectorWithSubscript,
        View(..),
        Generator(..),
        Reporter(..),
        DirectoryName(..),
        resolveDirectoryName,
        FileName(..),
        resolveFileName) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Concurrent.ParallelIO.Local

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA

import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IO

import Data.Ix
import Data.Maybe
import Data.Monoid

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
import Simulation.Aivika.Statistics

import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

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
               experimentIndexHtml     :: Experiment -> [Reporter] -> FilePath -> IO (),
               -- ^ Create the @index.html@ file after the simulation is finished
               -- in the specified directory.
               experimentNumCapabilities :: IO Int
               -- ^ The number of threads used for the Monte-Carlo simulation
               -- if the executable was compiled with the support of multi-threading.
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
               experimentIndexHtml     = createIndexHtml,
               experimentNumCapabilities = getNumCapabilities }

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
--
-- The array and list of series are treated as a sequence of
-- separate sub-series that have a subscript which can be
-- optionally specified explicitly. By default, the subscript
-- is numeric but it may be any string.
--  
-- At the same time, if the array or list of numeric values
-- is wrapped in monad 'Simulation' or 'Dynamics' then the
-- underlying numeric array and list are already treated as
-- a sampling statistics or list of numbers in time point.
--
-- Moreover, if the array or list of numbers is contained in
-- reference 'Ref' or variable 'Var' then the array and list
-- of such values are also treated as a sampling statistics
-- or list of numbers in time point.
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
                   providerToDoubleStats :: Maybe (Dynamics (SamplingStats Double)),
                   -- ^ Try to return the statistics data in time points.
                   providerToDoubleList :: Maybe (Dynamics [Double]),
                   -- ^ Try to return the list of double values.
                   providerToInt :: Maybe (Dynamics Int),
                   -- ^ Try to return the data as integers.
                   providerToIntStats :: Maybe (Dynamics (SamplingStats Int)),
                   -- ^ Try to return the statistics data in time points.
                   providerToIntList :: Maybe (Dynamics [Int]),
                   -- ^ Try to return the list of integer values.
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
--
-- In case of need you might want to specify the number of
-- threads directly with help of 'experimentNumCapabilities',
-- although the real number of parallel threads can depend on many
-- factors.
runExperimentParallel :: Experiment -> Simulation ExperimentData -> IO ()
runExperimentParallel e = runExperimentWithExecutor executor e 
  where executor tasks =
          do n <- experimentNumCapabilities e
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

-- | Represent a container for simulation data.
class SeriesContainer c where

  -- | Extract data from the container.
  containerData :: c a -> Dynamics a

  -- | Get the signal for the container.
  containerSignal :: c a => Maybe (Signal ())

instance SeriesContainer Identity where

  containerData = return . runIdentity

  containerSignal = const Nothing

instance SeriesContainer Simulation where

  containerData = liftSimulation

  containerSignal = const Nothing

instance SeriesContainer Dynamics where

  containerData = id

  containerSignal = const Nothing

instance SeriesContainer Ref where

  containerData = readRef

  containerSignal = Just . refChanged_

instance SeriesContainer Var where

  containerData = readVar

  containerSignal = Just . varChanged_

instance SeriesContainer c => Series (c Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble =
                                          Just $
                                          containerData s,
                                        providerToDoubleStats =
                                          Just $
                                          fmap returnSamplingStats $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap return $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
                                        providerSignal =
                                          containerSignal s }] }

instance SeriesContainer c => Series (c Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble =
                                          Just $
                                          fmap fromIntegral $
                                          containerData s,
                                        providerToDoubleStats =
                                          Just $
                                          fmap returnSamplingStats $
                                          fmap fromIntegral $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap return $
                                          fmap fromIntegral $
                                          containerData s,
                                        providerToInt =
                                          Just $
                                          containerData s,
                                        providerToIntStats =
                                          Just $
                                          fmap returnSamplingStats $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          fmap return $
                                          containerData s,
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
                                        providerSignal =
                                          containerSignal s }] }

instance SeriesContainer c => Series (c String) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats = Nothing,
                                        providerToDoubleList = Nothing,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString =
                                          Just $
                                          containerData s,
                                        providerSignal =
                                          containerSignal s }] }

instance Series (UVar Double) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readUVar s,
                                        providerToDoubleStats =
                                          Just $ fmap returnSamplingStats (readUVar s),
                                        providerToDoubleList =
                                          Just $ fmap return (readUVar s),
                                        providerToInt    = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }

instance Series (UVar Int) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap fromIntegral (readUVar s),
                                        providerToDoubleStats =
                                          Just $
                                          fmap returnSamplingStats $
                                          fmap fromIntegral (readUVar s),
                                        providerToDoubleList =
                                          Just $
                                          fmap return $
                                          fmap fromIntegral (readUVar s),
                                        providerToInt    = Just $ readUVar s,
                                        providerToIntStats =
                                          Just $ fmap returnSamplingStats (readUVar s),
                                        providerToIntList =
                                          Just $ fmap return (readUVar s),
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }
    
instance Series s => Series [s] where
  
  seriesEntity name s = 
    SeriesEntity { seriesProviders = 
                      join $ forM (zip [0..] s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity name' s }
    
instance (Show i, Ix i, Series s) => Series (Array i s) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      join $ forM (A.assocs s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity name' s }

instance Series s => Series (V.Vector s) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      join $ forM (zip [0..] (V.toList s)) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity name' s }

-- | Represents a list with the specified subscript.
data SeriesListWithSubscript s =
  SeriesListWithSubscript { seriesList          :: [s],
                            seriesListSubscript :: [String] }

-- | Represents an array with the specified subscript.
data SeriesArrayWithSubscript i s =
  SeriesArrayWithSubscript { seriesArray          :: Array i s,
                             seriesArraySubscript :: Array i String }

-- | Represents a vector with the specified subscript.
data SeriesVectorWithSubscript s =
  SeriesVectorWithSubscript { seriesVector          :: V.Vector s,
                              seriesVectorSubscript :: V.Vector String }

-- | Add the specified subscript to the list.
seriesListWithSubscript :: Series s => [s] -> [String] -> SeriesListWithSubscript s
seriesListWithSubscript = SeriesListWithSubscript

-- | Add the specified subscript to the array.
seriesArrayWithSubscript :: (Ix i, Series s) => Array i s -> Array i String
                            -> SeriesArrayWithSubscript i s
seriesArrayWithSubscript = SeriesArrayWithSubscript

-- | Add the specified subscript to the vector.
seriesVectorWithSubscript :: Series s => V.Vector s -> V.Vector String
                             -> SeriesVectorWithSubscript s
seriesVectorWithSubscript = SeriesVectorWithSubscript

instance Series s => Series (SeriesListWithSubscript s) where
  
  seriesEntity name s = 
    SeriesEntity { seriesProviders = do
                      let xs = seriesList s
                          ns = seriesListSubscript s
                      join $ forM (zip3 [1..] xs ns) $ \(i, s, n) ->
                        let name' = name ++ n
                        in seriesProviders $ seriesEntity name' s }
    
instance (Ix i, Series s) => Series (SeriesArrayWithSubscript i s) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders = do
                      let xs = seriesArray s
                          ns = seriesArraySubscript s
                      join $ forM (zip (A.assocs xs) (A.elems ns)) $ \((i, s), n) ->
                        let name' = name ++ n
                        in seriesProviders $ seriesEntity name' s }

instance Series s => Series (SeriesVectorWithSubscript s) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders = do
                      let xs = seriesVector s
                          ns = seriesVectorSubscript s
                      join $ forM (zip (V.toList xs) (V.toList ns)) $ \(x, n) ->
                        let name' = name ++ n
                        in seriesProviders $ seriesEntity name' x }

instance SeriesContainer c => Series (c (SamplingStats Double)) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          containerData s,
                                        providerToDoubleList = Nothing,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c (SamplingStats Int)) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          containerData s,
                                        providerToDoubleList = Nothing,
                                        providerToInt    = Nothing,
                                        providerToIntStats =
                                          Just $
                                          containerData s,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c [Double]) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap listSamplingStats $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c [Int]) where
  
  seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          fmap listSamplingStats $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap (map fromIntegral) $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats =
                                          Just $
                                          fmap listSamplingStats $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          containerData s,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance (Ix i, SeriesContainer c) => Series (c (Array i Double)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap A.elems $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap A.elems $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance (Ix i, SeriesContainer c) => Series (c (Array i Int)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          fmap listSamplingStats $
                                          fmap A.elems $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap (map fromIntegral) $
                                          fmap A.elems $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap A.elems $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          fmap A.elems $
                                          containerData s,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance (Ix i, SeriesContainer c) => Series (c (UArray i Double)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance (Ix i, SeriesContainer c) => Series (c (UArray i Int)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          fmap listSamplingStats $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap (map fromIntegral) $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          fmap UA.elems $
                                          containerData s,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c (V.Vector Double)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap V.toList $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap V.toList $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c (V.Vector Int)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          fmap listSamplingStats $
                                          fmap V.toList $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap (map fromIntegral) $
                                          fmap V.toList $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap V.toList $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          fmap V.toList $
                                          containerData s,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s }] }

instance SeriesContainer c => Series (c (UV.Vector Double)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats = Nothing,
                                        providerToIntList = Nothing,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }

instance SeriesContainer c => Series (c (UV.Vector Int)) where

   seriesEntity name s =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName = name,
                                        providerToDouble = Nothing,
                                        providerToDoubleStats =
                                          Just $
                                          fmap fromIntSamplingStats $
                                          fmap listSamplingStats $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToDoubleList =
                                          Just $
                                          fmap (map fromIntegral) $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToInt = Nothing,
                                        providerToIntStats =
                                          Just $
                                          fmap listSamplingStats $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToIntList =
                                          Just $
                                          fmap UV.toList $
                                          containerData s,
                                        providerToString = Nothing,
                                        providerSignal =
                                          containerSignal s } ] }
