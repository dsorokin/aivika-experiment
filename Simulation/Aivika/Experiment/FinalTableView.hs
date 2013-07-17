
-- |
-- Module     : Simulation.Aivika.Experiment.FinalTableView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'FinalTableView' that saves the simulation
-- results in the final time points for all simulation runs in
-- the CSV file.
--

module Simulation.Aivika.Experiment.FinalTableView
       (FinalTableView(..), 
        defaultFinalTableView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import System.IO
import System.FilePath

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base

-- | Defines the 'View' that saves the simulation 
-- results in the final time points for all 
-- simulation runs in the CSV file.
data FinalTableView =
  FinalTableView { finalTableTitle       :: String,
                   -- ^ This is a title used in HTML.
                   finalTableDescription :: String,
                   -- ^ This is a description used in HTML.
                   finalTableRunText     :: String,
                   -- ^ Translated text \"Run\".
                   finalTableLinkText    :: String,
                   -- ^ It specifies the text for the link 
                   -- which is displayed in the HTML page. 
                   -- The link downloads the corresponded 
                   -- CSV file in the browser. 
                   --
                   -- An example is
                   --
                   -- @
                   --   finalTableLinkText = \"Download the CSV file\"
                   -- @
                   finalTableFileName    :: FileName,
                   -- ^ It defines the file name for the CSV file. 
                   -- It may include special variable @$TITLE@.
                   --
                   -- An example is
                   --
                   -- @
                   --   finalTableFileName = UniqueFileName \"$TITLE\", \".csv\"
                   -- @
                   finalTableSeparator   :: String,
                   -- ^ It defines the separator for the view. 
                   -- It delimits the cells in the rows of the CSV file.
                   finalTableFormatter   :: ShowS,
                   -- ^ It defines the formatter which is applied
                   -- to all values before they will be written
                   -- in the CSV file.
                   finalTablePredicate   :: Dynamics Bool,
                   -- ^ It specifies the predicate that defines
                   -- when we can save data in the table.
                   finalTableSeries      :: [String] 
                   -- ^ It contains the labels of data saved
                   -- in the CSV file.
                 }
  
-- | The default table view.  
defaultFinalTableView :: FinalTableView
defaultFinalTableView = 
  FinalTableView { finalTableTitle       = "Final Table",
                   finalTableDescription = "It refers to the CSV file with the results in the final time points.",
                   finalTableRunText     = "Run",
                   finalTableLinkText    = "Download the CSV file",
                   finalTableFileName    = UniqueFileName "$TITLE" ".csv",
                   finalTableSeparator   = ",",
                   finalTableFormatter   = id,
                   finalTablePredicate   = return True,
                   finalTableSeries      = [] }

instance View FinalTableView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newFinalTable v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = finaliseFinalTable st,
                               reporterSimulate   = simulateFinalTable st,
                               reporterTOCHtml    = finalTableTOCHtml st,
                               reporterHtml       = finalTableHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data FinalTableViewState =
  FinalTableViewState { finalTableView       :: FinalTableView,
                        finalTableExperiment :: Experiment,
                        finalTableDir        :: FilePath, 
                        finalTableFile       :: IORef (Maybe FilePath),
                        finalTableLock       :: MVar (),
                        finalTableResults    :: IORef (Maybe FinalTableResults) }

-- | The table results.
data FinalTableResults =
  FinalTableResults { finalTableNames  :: [String],
                      finalTableValues :: IORef (M.Map Int [String]) }
  
-- | Create a new state of the view.
newFinalTable :: FinalTableView -> Experiment -> FilePath -> IO FinalTableViewState
newFinalTable view exp dir =
  do f <- newIORef Nothing
     l <- newMVar () 
     r <- newIORef Nothing
     return FinalTableViewState { finalTableView       = view,
                                  finalTableExperiment = exp,
                                  finalTableDir        = dir, 
                                  finalTableFile       = f,
                                  finalTableLock       = l, 
                                  finalTableResults    = r }
       
-- | Create new table results.
newFinalTableResults :: [String] -> Experiment -> IO FinalTableResults
newFinalTableResults names exp =
  do values <- newIORef M.empty 
     return FinalTableResults { finalTableNames  = names,
                                finalTableValues = values }
       
-- | Simulation of the specified series.
simulateFinalTable :: FinalTableViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateFinalTable st expdata =
  do let labels = finalTableSeries $ finalTableView st
         providers = experimentSeriesProviders expdata labels
         input =
           flip map providers $ \provider ->
           case providerToString provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as string values: simulateFinalTable"
             Just input -> input
         names = map providerName providers
         predicate = finalTablePredicate $ finalTableView st
         exp = finalTableExperiment st
         lock = finalTableLock st
     results <- liftIO $ readIORef (finalTableResults st)
     case results of
       Nothing ->
         liftIO $
         do results <- newFinalTableResults names exp
            writeIORef (finalTableResults st) $ Just results
       Just results ->
         when (names /= finalTableNames results) $
         error "Series with different names are returned for different runs: simulateFinalTable"
     results <- liftIO $ fmap fromJust $ readIORef (finalTableResults st)
     let values = finalTableValues results
     t <- time
     enqueue (experimentQueue expdata) t $
       do let h = filterSignalM (const predicate) $
                  experimentSignalInStopTime expdata
          -- we must subscribe through the event queue;
          -- otherwise, we will loose a signal in the start time,
          -- because the handleSignal_ function checks the event queue
          handleSignal_ h $ \_ ->
            do xs <- sequence input
               i  <- liftSimulation simulationIndex
               liftIO $ withMVar lock $ \() ->
                 modifyIORef values $ M.insert i xs
     return $ return ()
     
-- | Save the results in the CSV file after the simulation is complete.
finaliseFinalTable :: FinalTableViewState -> IO ()
finaliseFinalTable st =
  do let run       = finalTableRunText $ finalTableView st
         formatter = finalTableFormatter $ finalTableView st
         title     = finalTableTitle $ finalTableView st
         separator = finalTableSeparator $ finalTableView st
     results <- readIORef $ finalTableResults st
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalTableNames results
                values = finalTableValues results
            m <- readIORef values 
            file <- resolveFileName 
                    (Just $ finalTableDir st)
                    (finalTableFileName $ finalTableView st) $
                    M.fromList [("$TITLE", title)]
            -- create a new file
            h <- liftIO $ openFile file WriteMode
            -- write a header
            hPutStr h run
            forM_ names $ \name ->
              do hPutStr h separator
                 hPutStr h name
            hPutStrLn h ""
            -- write data
            forM_ (M.assocs m) $ \(i, xs) ->
              do hPutStr h $ show i
                 forM_ xs $ \x ->
                   do hPutStr h separator
                      hPutStr h $ formatter x
                 hPutStrLn h ""
            -- close file
            hClose h 
            when (experimentVerbose $ finalTableExperiment st) $
              putStr "Generated file " >> putStrLn file
            writeIORef (finalTableFile st) $ Just file
     
-- | Get the HTML code.     
finalTableHtml :: FinalTableViewState -> Int -> HtmlWriter ()
finalTableHtml st index =
  do header st index
     file <- liftIO $ readIORef (finalTableFile st)
     case file of
       Nothing -> return ()
       Just f  ->
         writeHtmlParagraph $
         writeHtmlLink (makeRelative (finalTableDir st) f) $
         writeHtmlText (finalTableLinkText $ finalTableView st)

header :: FinalTableViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalTableTitle $ finalTableView st)
     let description = finalTableDescription $ finalTableView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalTableTOCHtml :: FinalTableViewState -> Int -> HtmlWriter ()
finalTableTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalTableTitle $ finalTableView st)