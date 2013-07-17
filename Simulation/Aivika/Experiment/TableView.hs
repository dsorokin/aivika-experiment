
-- |
-- Module     : Simulation.Aivika.Experiment.TableView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'TableView' that saves the simulation
-- results in the CSV file(s).
--

module Simulation.Aivika.Experiment.TableView 
       (TableView(..), 
        defaultTableView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import System.IO
import System.FilePath

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base

-- | Defines the 'View' that saves the simulation results
-- in the CSV file(s).
data TableView =
  TableView { tableTitle       :: String,
              -- ^ This is a title used in HTML.
              tableDescription :: String,
              -- ^ This is a description in the HTML.
              tableLinkText    :: String,
              -- ^ It specifies the text for the link 
              -- which is displayed in the HTML page
              -- if there is only one simulation run. 
              -- The link downloads the corresponded 
              -- CSV file in the browser. If there are
              -- more simulation runs, then this link 
              -- is not shown.
              --
              -- An example is
              --
              -- @
              --   tableLinkText = \"Download the CSV file\"
              -- @
              tableRunLinkText :: String,
              -- ^ It specifies the link text which is 
              -- displayed in the HTML page if there are 
              -- many simulation runs. Such a link downloads 
              -- the CSV file for the corresponded run. 
              -- To define the text, you can use special 
              -- variables @$LINK@, @$RUN_INDEX@ and @$RUN_COUNT@.
              --
              -- An example is 
              -- 
              -- @
              --   tableRunLinkText = \"$LINK / Run $RUN_INDEX of $RUN_COUNT\"
              -- @
              -- 
              -- If there is only one run, then the link of 
              -- this kind is not displayed. Instead, only one 
              -- link is shown, which text is defined by the 
              -- 'tableLinkText' field.
              tableFileName    :: FileName,
              -- ^ It defines the file name for each CSV file. 
              -- It may include special variables @$TITLE@, 
              -- @$RUN_INDEX@ and @$RUN_COUNT@.
              --
              -- An example is
              --
              -- @
              --   tableFileName = UniqueFileName \"$TITLE - $RUN_INDEX\", \".csv\"
              -- @
              tableSeparator   :: String,
              -- ^ It defines the separator for the view. 
              -- It delimits the cells in the rows of the CSV file.
              tableFormatter   :: ShowS,
              -- ^ It defines the formatter which is applied
              -- to all values before they will be written
              -- in the CSV file(s).
              tablePredicate   :: Dynamics Bool,
              -- ^ It specifies the predicate that defines
              -- when we can save data in the table.
              tableSeries      :: [String] 
              -- ^ It contains the labels of data saved
              -- in the CSV file(s).
            }
  
-- | The default table view.  
defaultTableView :: TableView
defaultTableView = 
  TableView { tableTitle       = "Table",
              tableDescription = "This section contains the CSV file(s) with the simulation results.",
              tableLinkText    = "Download the CSV file",
              tableRunLinkText = "$LINK / Run $RUN_INDEX of $RUN_COUNT",
              tableFileName    = UniqueFileName "$TITLE - $RUN_INDEX" ".csv",
              tableSeparator   = ",",
              tableFormatter   = id,
              tablePredicate   = return True,
              tableSeries      = [] }
  
instance View TableView where
  
  outputView v = 
    let reporter exp dir =
          do st <- newTable v exp dir
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateTable st,
                               reporterTOCHtml    = tableTOCHtml st,
                               reporterHtml       = tableHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data TableViewState =
  TableViewState { tableView       :: TableView,
                   tableExperiment :: Experiment,
                   tableDir        :: FilePath, 
                   tableMap        :: M.Map Int FilePath }
  
-- | Create a new state of the view.
newTable :: TableView -> Experiment -> FilePath -> IO TableViewState
newTable view exp dir =
  do let n = experimentRunCount exp
     fs <- forM [0..(n - 1)] $ \i -> 
       resolveFileName (Just dir) (tableFileName view) $
       M.fromList [("$TITLE", tableTitle view),
                   ("$RUN_INDEX", show $ i + 1),
                   ("$RUN_COUNT", show n)]
     forM_ fs $ flip writeFile []  -- reserve the file names
     let m = M.fromList $ zip [0..(n - 1)] fs
     return TableViewState { tableView       = view,
                             tableExperiment = exp,
                             tableDir          = dir, 
                             tableMap          = m }
       
-- | Write the tables during the simulation.
simulateTable :: TableViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateTable st expdata =
  do let labels = tableSeries $ tableView st
         providers = experimentSeriesProviders expdata labels
         input =
           flip map providers $ \provider ->
           case providerToString provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as a string: simulateTable"
             Just input -> input
         separator = tableSeparator $ tableView st
         formatter = tableFormatter $ tableView st
         predicate = tablePredicate $ tableView st
     i <- liftSimulation simulationIndex
     -- create a new file
     let f = fromJust $ M.lookup (i - 1) (tableMap st)
     h <- liftIO $ openFile f WriteMode
     -- write a header
     liftIO $
       do forM_ (zip [0..] providers) $ \(column, provider) ->
            do when (column > 0) $ 
                 hPutStr h separator
               hPutStr h $ show $ providerName provider
          hPutStrLn h ""
     t <- time
     enqueue (experimentQueue expdata) t $
       -- we must subscribe through the event queue;
       -- otherwise, we will loose a signal in the start time,
       -- because the handleSignal_ function checks the event queue
       handleSignal_ (experimentMixedSignal expdata providers) $ \t ->
       do p <- predicate
          when p $
            do forM_ (zip [0..] input) $ \(column, input) ->  -- write the row
                 do x <- input                                -- write the column
                    liftIO $
                      do when (column > 0) $ 
                           hPutStr h separator
                         hPutStr h $ formatter x
               liftIO $ hPutStrLn h ""
     return $ 
       liftIO $
       do when (experimentVerbose $ tableExperiment st) $
            putStr "Generated file " >> putStrLn f
          hClose h  -- close the file
     
-- | Get the HTML code.     
tableHtml :: TableViewState -> Int -> HtmlWriter ()     
tableHtml st index =
  let n = experimentRunCount $ tableExperiment st
  in if n == 1
     then tableHtmlSingle st index
     else tableHtmlMultiple st index
     
-- | Get the HTML code for a single run.
tableHtmlSingle :: TableViewState -> Int -> HtmlWriter ()
tableHtmlSingle st index =
  do header st index
     let f = fromJust $ M.lookup 0 (tableMap st)
     writeHtmlParagraph $
       writeHtmlLink (makeRelative (tableDir st) f) $
       writeHtmlText (tableLinkText $ tableView st)

-- | Get the HTML code for multiple runs
tableHtmlMultiple :: TableViewState -> Int -> HtmlWriter ()
tableHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ tableExperiment st
     forM_ [0..(n - 1)] $ \i ->
       do let f = fromJust $ M.lookup i (tableMap st)
              sublink = 
                replace "$RUN_INDEX" (show $ i + 1) $
                replace "$RUN_COUNT" (show n) $
                replace "$LINK" (tableLinkText $ tableView st)
                (tableRunLinkText $ tableView st)
          writeHtmlParagraph $
            writeHtmlLink (makeRelative (tableDir st) f) $
            writeHtmlText sublink

header :: TableViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (tableTitle $ tableView st)
     let description = tableDescription $ tableView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item     
tableTOCHtml :: TableViewState -> Int -> HtmlWriter ()
tableTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (tableTitle $ tableView st)
