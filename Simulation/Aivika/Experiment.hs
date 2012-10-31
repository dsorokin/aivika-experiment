
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
       (Experiment,
        runExperiment,
        setExperimentSpecs,
        getExperimentSpecs,
        setExperimentRunCount,
        getExperimentRunCount,
        setExperimentDirectoryName,
        getExperimentDirectoryName,
        setExperimentTitle,
        getExperimentTitle,
        setExperimentDescription,
        getExperimentDescription,
        outputView,
        FileName(..),
        resolveFileName,
        ExperimentData(..),
        experimentDataInStartTime,
        experimentSeriesProviders,
        Series(..),
        SeriesSource,
        addDataSeries,
        addDataSeriesWithName,
        SeriesEntity(..),
        SeriesProvider(..),
        Task(..),
        initTask,
        View(..),
        Generator(..),
        Reporter(..)) where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.Array
import Data.Maybe
import Data.Monoid
import Data.String.Utils (replace)

import System.IO
import System.Directory

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.Ref
import Simulation.Aivika.Dynamics.Var
import Simulation.Aivika.Dynamics.UVar
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Parameter

import Simulation.Aivika.Experiment.HtmlWriter

-- | It defines the experiment task and reflects the functions 
-- within the 'Experiment' computation.
data Task = 
  Task { taskSpecs         :: Specs,
         -- ^ The simulation specs for the experiment.
         taskRunCount      :: Int,
         -- ^ How many simulation runs should be launched.
         taskDirectoryName :: FileName,
         -- ^ The directory in which the output results should be saved.
         taskTitle         :: String,
         -- ^ The experiment title.
         taskDescription   :: String,
         -- ^ The experiment description.
         taskGenerators    :: [Generator], 
         -- ^ The experiment generators.
         taskIndexHtml :: Task -> [Reporter] -> FilePath -> IO ()
         -- ^ Create the @index.html@ file after the simulation is finished
         -- in the specified directory. 
       }

-- | The initial experiment task.
initTask :: Task
initTask =
  Task { taskSpecs         = Specs 0 10 0.01 RungeKutta4,
         taskRunCount      = 1,
         taskDirectoryName = UniqueFileName "experiment",
         taskTitle         = "Simulation Experiment",
         taskDescription   = "",
         taskGenerators    = [], 
         taskIndexHtml     = createIndexHtml }

-- | Represents a computation whithin which we define a simulation 'Task'
-- without actual providing simulatable data.
type Experiment a = State Task a 
  
-- | Set the simulation specs.
setExperimentSpecs :: Specs -> Experiment ()
setExperimentSpecs specs =
  state $ \task -> ((), task { taskSpecs = specs })
                        
-- | Set how many simulation runs should be launched.                        
setExperimentRunCount :: Int -> Experiment ()
setExperimentRunCount n =
  state $ \task -> ((), task { taskRunCount = n })
                        
-- | Set where to save the results of simulation.                        
setExperimentDirectoryName :: FileName -> Experiment ()
setExperimentDirectoryName name =
  state $ \task -> ((), task { taskDirectoryName = name })
                        
-- | Set the experiment title.                        
setExperimentTitle :: String -> Experiment ()
setExperimentTitle name =
  state $ \task -> ((), task { taskTitle = name })

-- | Set the experiment description.
setExperimentDescription :: String -> Experiment ()
setExperimentDescription text = 
  state $ \task -> ((), task { taskDescription = text })
  
-- | Get the simulation specs.
getExperimentSpecs :: Experiment Specs
getExperimentSpecs =
  state $ \task -> (taskSpecs task, task)
                        
-- | Return how many simulation runs should be launched.                        
getExperimentRunCount :: Experiment Int
getExperimentRunCount =
  state $ \task -> (taskRunCount task, task)
                        
-- | Return where to save the results of simulation.                        
getExperimentDirectoryName :: Experiment FileName
getExperimentDirectoryName =
  state $ \task -> (taskDirectoryName task, task)
                        
-- | Get the experiment title.                        
getExperimentTitle :: Experiment String
getExperimentTitle =
  state $ \task -> (taskTitle task, task)

-- | Get the experiment description.
getExperimentDescription :: Experiment String
getExperimentDescription = 
  state $ \task -> (taskDescription task, task)

-- | This is a generator of the reporter.                     
data Generator = 
  Generator { generateReporter :: Task -> FilePath -> IO Reporter 
              -- ^ Generate a reporter for the specified directory,
              -- where the index.html file will be saved for the 
              -- current simulation experiment.
            }

-- | Defines a view in which the simulation results should be saved.
-- You should extend this type class to define your own views such
-- as the PDF document.
class View v where
  
  -- | Return the initial view.
  initView :: v
  
  -- | Create a generator of the reporter.
  viewGenerator :: v -> Generator
  
-- | Output the view within the 'Experiment' computation.
outputView :: View v => State v () -> Experiment ()
outputView output =
  state $ \task -> 
  let ((), view) = runState output initView
      generator  = viewGenerator view
      generators = generator : taskGenerators task
  in ((), task { taskGenerators = generators })

-- | Represents the series. It is usually something, or
-- an array of something, or a list of such values which
-- can be simulated.
class Series s where
  
  -- | Return the simulatable entity with the specified name
  -- for the given series.
  seriesEntity :: s -> String -> SeriesEntity
  
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

-- | This computation collects the 'SeriesEntity' values received
-- from the 'Series' instances. 
type SeriesSource a = State (M.Map String SeriesEntity) a
       
-- | Add the series with the specified label to the computation of sources,
-- where the name is the same as the label.
addDataSeries :: Series s => String -> s -> SeriesSource ()
addDataSeries label s = 
  state (\m -> ((), M.insert label (seriesEntity s label) m))

-- | Add the series with the specified name and label to the computation of sources.
-- The name goes first but the label is the second argument.
addDataSeriesWithName :: Series s => String -> String -> s -> SeriesSource ()
addDataSeriesWithName name label s = 
  state (\m -> ((), M.insert label (seriesEntity s name) m))

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
                   experimentCompoundSignal :: Signal (),
                   -- ^ The signal triggered on every change in the data.
                   experimentSeries :: M.Map String SeriesEntity
                   -- ^ The simulation entitities with labels as keys.
                 }

-- | Prepare data for the simulation experiment in start time from the source.
experimentDataInStartTime :: EventQueue -> SeriesSource () -> Simulation ExperimentData
experimentDataInStartTime q m = runDynamicsInStartTime d where
  d = do signalInIntegTimes <- newSignalInIntegTimes q
         signalInStartTime  <- newSignalInStartTime q
         signalInStopTime   <- newSignalInStopTime q
         let ((), series) = runState m M.empty
             xs0 = map providerSignal $ 
                   join $ map seriesProviders $ 
                   M.elems series
             xs1 = filter isJust xs0
             xs2 = filter isNothing xs0
             signal1 = mconcat $ map fromJust xs1
             signal2 = if (null xs2) 
                       then emptySignal 
                       else fmap (const ()) signalInIntegTimes
             signal3 = fmap (const ()) signalInStopTime
             compoundSignal = signal1 <> signal2 <> signal3
         return ExperimentData { experimentQueue              = q,
                                 experimentSignalInIntegTimes = signalInIntegTimes,
                                 experimentSignalInStartTime  = signalInStartTime,
                                 experimentSignalInStopTime   = signalInStopTime,
                                 experimentCompoundSignal     = compoundSignal,
                                 experimentSeries             = series }

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
             reporterHtml :: Int -> HtmlWriter ()
             -- ^ Return an HTML code for the index file
             -- after the finalisation function is called,
             -- i.e. in the very end. The agument specifies
             -- the ordered number of the item.
           }

-- | Run the simulation experiment sequentially. For example, 
-- it can be a Monte-Carlo simulation dependentent on the external
-- 'Parameter' values.
runExperiment :: Experiment () -> Simulation ExperimentData -> IO ()
runExperiment e simulation = 
  do let ((), task) = runState e initTask
         specs      = taskSpecs task
         runCount   = taskRunCount task
         dirName    = taskDirectoryName task
         generators = reverse $ taskGenerators task
     path <- resolveFileName dirName M.empty
     putStrLn $ "Using directory " ++ path
     createDirectoryIfMissing True path
     reporters <- mapM (\x -> generateReporter x task path) $
                  generators
     forM_ reporters reporterInitialise
     let simulate :: Simulation ()
         simulate =
           do d  <- simulation
              fs <- runDynamicsInStartTime $
                    forM reporters $ \reporter ->
                    reporterSimulate reporter d
              runDynamicsInStopTime $
                do updateSignal $ experimentCompoundSignal d
                   sequence_ fs
     sequence_ $ runSimulations simulate specs runCount
     forM_ reporters reporterFinalise
     taskIndexHtml task task reporters path
     return ()
     
-- | Create an index HTML file.     
createIndexHtml :: Task -> [Reporter] -> FilePath -> IO ()
createIndexHtml task reporters path = 
  do let header :: HtmlWriter ()
         header = do
           writeHtml "<html>"
           writeHtml "<head>"
           writeHtml "<title>"
           writeHtmlText $ taskTitle task
           writeHtml "</title>"
           writeHtml "</head>"
           writeHtml "<body>"
           writeHtml "<h1>"
           writeHtmlText $ taskTitle task
           writeHtml "</h1>"
         footer :: HtmlWriter ()
         footer = do
           writeHtml "<br /><p><font size=\"-1\">Automatically generated by "
           writeHtml "<a href=\"http://hackage.haskell.org/package/aivika\">"
           writeHtml "Aivika</a>"
           writeHtml "</font></p>"
           writeHtml "</body>"
           writeHtml "</html>"
         contents :: (Reporter -> Int -> HtmlWriter ()) -> HtmlWriter ()
         contents f =
           forM_ (zip [1..] reporters) $ \(i, reporter) ->
           f reporter i
         html :: HtmlWriter ()
         html = do 
           header
           contents reporterTOCHtml
           writeHtml "<br />"
           writeHtmlText $ taskDescription task
           contents reporterHtml
           footer
     ((), contents) <- runHtmlWriter html id
     writeFile (path ++ "/index.html") (contents [])
     putStr "Generated file "
     putStr path
     putStrLn "/index.html"

-- | Specifies the file name, unique or writable.
data FileName = WritableFileName String
                -- ^ The file which is overwritten in 
                -- case if it existed before.
              | UniqueFileName String
                -- ^ The file which is always unique,
                -- when a prefix is added to the name
                -- in case of need.
                
-- | Resolve file name replacing the specified strings.             
resolveFileName :: FileName -> M.Map String String -> IO String
resolveFileName (WritableFileName name) map = 
  return $ replaceName name map
resolveFileName (UniqueFileName name) map =
  let x = replaceName name map
      loop y i =
        do f1 <- doesFileExist y
           f2 <- doesDirectoryExist y
           if f1 || f2
             then loop (x ++ "(" ++ show i ++ ")") (i + 1)
             else return y
  in loop x 2
     
-- | Replace the name according the specified table.
replaceName :: String -> M.Map String String -> String     
replaceName name map = name' where
  ((), name') = flip runState name $
                forM_ (M.assocs map) $ \(k, v) ->
                do a <- get
                   put $ replace k v a

instance Series (Dynamics Double) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Dynamics Int) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap (fromInteger . toInteger) s,
                                        providerToInt    = Just s,
                                        providerToString = Just $ fmap show s,
                                        providerSignal   = Nothing }] }

instance Series (Dynamics String) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just s,
                                        providerSignal   = Nothing }] }

instance Series (Ref Double) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readRef s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readRef s),
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Ref Int) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap (fromInteger . toInteger) (readRef s),
                                        providerToInt    = Just $ readRef s,
                                        providerToString = Just $ fmap show (readRef s),
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Ref String) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ readRef s,
                                        providerSignal   = Just $ refChanged_ s }] }

instance Series (Var Double) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readVar s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readVar s),
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (Var Int) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap (fromInteger . toInteger) (readVar s),
                                        providerToInt    = Just $ readVar s,
                                        providerToString = Just $ fmap show (readVar s),
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (Var String) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Nothing,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ readVar s,
                                        providerSignal   = Just $ varChanged_ s }] }

instance Series (UVar Double) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ readUVar s,
                                        providerToInt    = Nothing,
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }

instance Series (UVar Int) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      [SeriesProvider { providerName     = name,
                                        providerToDouble = Just $ fmap (fromInteger . toInteger) (readUVar s),
                                        providerToInt    = Just $ readUVar s,
                                        providerToString = Just $ fmap show (readUVar s),
                                        providerSignal   = Just $ uvarChanged_ s }] }
    
instance Series s => Series [s] where
  
  seriesEntity s name = 
    SeriesEntity { seriesProviders = 
                      join $ forM (zip [1..] s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity s name' }
    
instance (Show i, Ix i, Series s) => Series (Array i s) where
  
  seriesEntity s name =
    SeriesEntity { seriesProviders =
                      join $ forM (assocs s) $ \(i, s) ->
                      let name' = name ++ "[" ++ show i ++ "]"
                      in seriesProviders $ seriesEntity s name' }
