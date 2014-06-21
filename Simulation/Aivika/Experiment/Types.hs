
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
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

module Simulation.Aivika.Experiment.Types
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
        ExperimentView(..),
        ExperimentGenerator(..),
        ExperimentReporter(..),
        ExperimentFilePath(..),
        resolveFilePath,
        expandFilePath) where

import Control.Monad
import Control.Monad.State
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
import System.FilePath

import GHC.Conc (getNumCapabilities)

import Simulation.Aivika hiding (Var, readVar, varChanged_)
import Simulation.Aivika.Var
import qualified Simulation.Aivika.Ref.Light as LR

import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

-- | It defines the simulation experiment with the specified rendering backend.
data Experiment r = 
  Experiment { experimentSpecs         :: Specs,
               -- ^ The simulation specs for the experiment.
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
               experimentGenerators    :: [ExperimentGenerator r], 
               -- ^ The experiment generators.
               experimentIndexHtml     :: Experiment r -> [ExperimentReporter] -> FilePath -> IO (),
               -- ^ Create the @index.html@ file after the simulation is finished
               -- in the specified directory.
               experimentNumCapabilities :: IO Int
               -- ^ The number of threads used for the Monte-Carlo simulation
               -- if the executable was compiled with the support of multi-threading.
             }

-- | The default experiment.
defaultExperiment :: Experiment r
defaultExperiment =
  Experiment { experimentSpecs         = Specs 0 10 0.01 RungeKutta4 SimpleGenerator,
               experimentRunCount      = 1,
               experimentDirectoryName = UniqueFilePath "experiment",
               experimentTitle         = "Simulation Experiment",
               experimentDescription   = "",
               experimentVerbose       = True,
               experimentGenerators    = [], 
               experimentIndexHtml     = createIndexHtml,
               experimentNumCapabilities = getNumCapabilities }

-- | This is a generator of the reporter with the specified rendering backend.                     
data ExperimentGenerator r = 
  ExperimentGenerator { generateReporter :: Experiment r -> r -> FilePath -> IO ExperimentReporter 
                        -- ^ Generate a reporter for the specified directory,
                        -- where the @index.html@ file will be saved for the 
                        -- current simulation experiment.
                      }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class ExperimentView v r where
  
  -- | Create a generator of the reporter.
  outputView :: v -> ExperimentGenerator r
  
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
-- is wrapped in monads 'Simulation', 'Dynamics' or 'Event' then the
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
                   providerToDouble :: Maybe (Event Double),
                   -- ^ Try to return the data as double values.
                   providerToDoubleStats :: Maybe (Event (SamplingStats Double)),
                   -- ^ Try to return the statistics data in time points.
                   providerToDoubleList :: Maybe (Event [Double]),
                   -- ^ Try to return the list of double values.
                   providerToInt :: Maybe (Event Int),
                   -- ^ Try to return the data as integers.
                   providerToIntStats :: Maybe (Event (SamplingStats Int)),
                   -- ^ Try to return the statistics data in time points.
                   providerToIntList :: Maybe (Event [Int]),
                   -- ^ Try to return the list of integer values.
                   providerToString :: Maybe (Event String),
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
  ExperimentData { experimentSignalInIntegTimes :: Signal Double,
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
experimentDataInStartTime :: [(String, SeriesEntity)] -> Simulation ExperimentData
experimentDataInStartTime m = runDynamicsInStartTime $ runEventWith EarlierEvents d where
  d = do signalInIntegTimes <- newSignalInIntegTimes
         signalInStartTime  <- newSignalInStartTime
         signalInStopTime   <- newSignalInStopTime
         let series = M.fromList m
         return ExperimentData { experimentSignalInIntegTimes = signalInIntegTimes,
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
data ExperimentReporter =
  ExperimentReporter { reporterInitialise :: IO (),
                       -- ^ Initialise the reporting before 
                       -- the simulation runs are started.
                       reporterFinalise   :: IO (),
                       -- ^ Finalise the reporting after
                       -- all simulation runs are finished.
                       reporterSimulate   :: ExperimentData -> Event (Event ()),
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
runExperiment :: Experiment r -> r -> Simulation ExperimentData -> IO ()
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
runExperimentParallel :: Experiment r -> r -> Simulation ExperimentData -> IO ()
runExperimentParallel e = runExperimentWithExecutor executor e 
  where executor tasks =
          do n <- experimentNumCapabilities e
             withPool n $ \pool ->
               parallel_ pool tasks
                        
-- | Run the simulation experiment with the specified executor.
runExperimentWithExecutor :: ([IO ()] -> IO ()) ->
                             Experiment r -> r -> 
                             Simulation ExperimentData -> IO ()
runExperimentWithExecutor executor e r simulation = 
  do let specs      = experimentSpecs e
         runCount   = experimentRunCount e
         dirName    = experimentDirectoryName e
         generators = experimentGenerators e
     path <- resolveFilePath "" dirName
     when (experimentVerbose e) $
       do putStr "Using directory " 
          putStrLn path
     createDirectoryIfMissing True path
     reporters <- mapM (\x -> generateReporter x e r path)
                  generators
     forM_ reporters reporterInitialise
     let simulate :: Simulation ()
         simulate =
           do d  <- simulation
              fs <- runDynamicsInStartTime $
                    runEventWith EarlierEvents $
                    forM reporters $ \reporter ->
                    reporterSimulate reporter d
              runEventInStopTime $
                sequence_ fs
     executor $ runSimulations simulate specs runCount
     forM_ reporters reporterFinalise
     experimentIndexHtml e e reporters path
     return ()
     
-- | Create an index HTML file.     
createIndexHtml :: Experiment r -> [ExperimentReporter] -> FilePath -> IO ()
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

-- | Represent a container for simulation data.
class SeriesContainer c where

  -- | Extract data from the container.
  containerData :: c a -> Event a

  -- | Get the signal for the container.
  containerSignal :: c a => Maybe (Signal ())

instance SeriesContainer Parameter where

  containerData = liftParameter

  containerSignal = const Nothing

instance SeriesContainer Simulation where

  containerData = liftSimulation

  containerSignal = const Nothing

instance SeriesContainer Dynamics where

  containerData = liftDynamics

  containerSignal = const Nothing

instance SeriesContainer Event where

  containerData = id

  containerSignal = const Nothing

instance SeriesContainer Ref where

  containerData = readRef

  containerSignal = Just . refChanged_

instance SeriesContainer LR.Ref where

  containerData = LR.readRef

  containerSignal = const Nothing

instance SeriesContainer Var where

  containerData = readVar

  containerSignal = Just . varChanged_

instance SeriesContainer Signalable where

  containerData = readSignalable

  containerSignal = Just . signalableChanged_

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
seriesListWithSubscript :: Series s
                           => [s]
                           -- ^ the list to subscript
                           -> [String]
                           -- ^ the list of subscripts
                           -> SeriesListWithSubscript s
                           -- ^ the subscripted list
seriesListWithSubscript = SeriesListWithSubscript

-- | Add the specified subscript to the array.
seriesArrayWithSubscript :: (Ix i, Series s)
                            => Array i s
                            -- ^ the array to subscript
                            -> Array i String
                            -- ^ the array of subscripts
                            -> SeriesArrayWithSubscript i s
                            -- ^ the subscripted array
seriesArrayWithSubscript = SeriesArrayWithSubscript

-- | Add the specified subscript to the vector.
seriesVectorWithSubscript :: Series s
                             => V.Vector s
                             -- ^ the vector to subscript
                             -> V.Vector String
                             -- ^ the vector of subscripts
                             -> SeriesVectorWithSubscript s
                             -- ^ the subscripted vector
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
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
                                        providerToString =
                                          Just $
                                          fmap show $
                                          containerData s,
                                        providerSignal =
                                          containerSignal s } ] }
