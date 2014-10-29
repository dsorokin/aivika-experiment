
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.FinalTableView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.ExperimentWriter
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.MRef

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
                   finalTableFileName    :: ExperimentFilePath,
                   -- ^ It defines the file name for the CSV file. 
                   -- It may include special variable @$TITLE@.
                   --
                   -- An example is
                   --
                   -- @
                   --   finalTableFileName = UniqueFilePath \"$TITLE.csv\"
                   -- @
                   finalTableSeparator   :: String,
                   -- ^ It defines the separator for the view. 
                   -- It delimits the cells in the rows of the CSV file.
                   finalTableFormatter   :: ShowS,
                   -- ^ It defines the formatter which is applied
                   -- to all values before they will be written
                   -- in the CSV file.
                   finalTablePredicate   :: Event Bool,
                   -- ^ It specifies the predicate that defines
                   -- when we can save data in the table.
                   finalTableTransform   :: ResultTransform,
                   -- ^ The transform applied to the results before receiving series.
                   finalTableSeries      :: ResultTransform 
                   -- ^ It defines the series to save in the CSV file.
                 }
  
-- | The default table view.  
defaultFinalTableView :: FinalTableView
defaultFinalTableView = 
  FinalTableView { finalTableTitle       = "Final Table",
                   finalTableDescription = "It refers to the CSV file with the results in the final time points.",
                   finalTableRunText     = "Run",
                   finalTableLinkText    = "Download the CSV file",
                   finalTableFileName    = UniqueFilePath "$TITLE.csv",
                   finalTableSeparator   = ",",
                   finalTableFormatter   = id,
                   finalTablePredicate   = return True,
                   finalTableTransform   = expandResults,
                   finalTableSeries      = id }

instance WebPageRendering r => ExperimentView FinalTableView r WebPageWriter where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newFinalTable v exp dir
             let writer =
                   WebPageWriter { reporterWriteTOCHtml = finalTableTOCHtml st,
                                   reporterWriteHtml    = finalTableHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = finaliseFinalTable st,
                                         reporterSimulate   = simulateFinalTable st,
                                         reporterRequest    = writer }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalTableViewState =
  FinalTableViewState { finalTableView       :: FinalTableView,
                        finalTableExperiment :: Experiment,
                        finalTableDir        :: FilePath, 
                        finalTableFile       :: IORef (Maybe FilePath),
                        finalTableResults    :: MRef (Maybe FinalTableResults) }

-- | The table results.
data FinalTableResults =
  FinalTableResults { finalTableNames  :: [String],
                      finalTableValues :: MRef (M.Map Int [String]) }
  
-- | Create a new state of the view.
newFinalTable :: FinalTableView -> Experiment -> FilePath -> ExperimentWriter FinalTableViewState
newFinalTable view exp dir =
  do f <- liftIO $ newIORef Nothing
     r <- liftIO $ newMRef Nothing
     return FinalTableViewState { finalTableView       = view,
                                  finalTableExperiment = exp,
                                  finalTableDir        = dir, 
                                  finalTableFile       = f,
                                  finalTableResults    = r }
       
-- | Create new table results.
newFinalTableResults :: [String] -> Experiment -> IO FinalTableResults
newFinalTableResults names exp =
  do values <- newMRef M.empty 
     return FinalTableResults { finalTableNames  = names,
                                finalTableValues = values }

-- | Require to return unique final tables results associated with the specified state. 
requireFinalTableResults :: FinalTableViewState -> [String] -> IO FinalTableResults
requireFinalTableResults st names =
  maybeWriteMRef (finalTableResults st)
  (newFinalTableResults names (finalTableExperiment st)) $ \results ->
  if (names /= finalTableNames results)
  then error "Series with different names are returned for different runs: requireFinalTableResults"
  else return results
       
-- | Simulation of the specified series.
simulateFinalTable :: FinalTableViewState -> ExperimentData -> Event DisposableEvent
simulateFinalTable st expdata =
  do let view    = finalTableView st
         rs      = finalTableSeries view $
                   finalTableTransform view $
                   experimentResults expdata
         exts    = extractStringResults rs
         signals = experimentPredefinedSignals expdata
         signal  = filterSignalM (const predicate) $
                   resultSignalInStopTime signals
         names   = map resultExtractName exts
         predicate = finalTablePredicate view
     results <- liftIO $ requireFinalTableResults st names
     let values = finalTableValues results 
     handleSignal signal $ \_ ->
       do xs <- mapM resultExtractData exts
          i  <- liftParameter simulationIndex
          liftIO $ modifyMRef_ values $ return . M.insert i xs
     
-- | Save the results in the CSV file after the simulation is complete.
finaliseFinalTable :: FinalTableViewState -> ExperimentWriter ()
finaliseFinalTable st =
  do let view      = finalTableView st
         run       = finalTableRunText view
         formatter = finalTableFormatter view
         title     = finalTableTitle view
         separator = finalTableSeparator view
     results <- liftIO $ readMRef $ finalTableResults st
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalTableNames results
                values = finalTableValues results
            m <- liftIO $ readMRef values 
            file <- resolveFilePath (finalTableDir st) $
                    mapFilePath (flip replaceExtension ".csv") $
                    expandFilePath (finalTableFileName $ finalTableView st) $
                    M.fromList [("$TITLE", title)]
            liftIO $ do
              -- create a new file
              h <- openFile file WriteMode
              -- write a header
              hPutStr h $ show run
              forM_ names $ \name ->
                do hPutStr h separator
                   hPutStr h $ show name
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
