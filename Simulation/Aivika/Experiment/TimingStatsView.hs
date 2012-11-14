
-- |
-- Module     : Simulation.Aivika.Experiment.TimingStatsView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'TimingStatsView' that shows the timing statistics
-- for the variables for every simulation run separately.
--

module Simulation.Aivika.Experiment.TimingStatsView 
       (TimingStatsView(..),
        defaultTimingStatsView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import Data.String.Utils (replace)

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.TimingStatsWriter

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal
import Simulation.Aivika.Dynamics.EventQueue
import Simulation.Aivika.Dynamics.Base
import Simulation.Aivika.Statistics

-- | Defines the 'View' that shows the timing statistics
-- for variables for every simulation run separately.
data TimingStatsView =
  TimingStatsView { timingStatsTitle       :: String,
                    -- ^ The title for the view.
                    timingStatsRunTitle    :: String,
                    -- ^ The run title for the view. It may include
                    -- special variables @$RUN_INDEX@, @$RUN_COUNT@ and 
                    -- @$TITLE@.
                    --
                    -- An example is 
                    --
                    -- @
                    --   timingStatsRunTitle = \"$TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                    -- @
                    timingStatsDescription :: String,
                    -- ^ The description for the view.
                    timingStatsWriter      :: TimingStatsWriter Double,
                    -- ^ It shows the timing statistics.
                    timingStatsSeries      :: [String] 
                    -- ^ It contains the labels of the observed series.
                  }
  
-- | This is the default view.
defaultTimingStatsView :: TimingStatsView
defaultTimingStatsView =  
  TimingStatsView { timingStatsTitle       = "Timing Statistics",
                    timingStatsRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                    timingStatsDescription = [],
                    timingStatsWriter      = defaultTimingStatsWriter,
                    timingStatsSeries      = [] }

instance View TimingStatsView where  
  
  outputView v = 
    let reporter exp dir =
          do st <- newTimingStats v exp
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateTimingStats st,
                               reporterTOCHtml    = timingStatsTOCHtml st,
                               reporterHtml       = timingStatsHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data TimingStatsViewState =
  TimingStatsViewState { timingStatsView       :: TimingStatsView,
                         timingStatsExperiment :: Experiment,
                         timingStatsMap        :: M.Map Int (IORef [(String, IORef (TimingStats Double))]) }
  
-- | Create a new state of the view.
newTimingStats :: TimingStatsView -> Experiment -> IO TimingStatsViewState
newTimingStats view exp =
  do let n = experimentRunCount exp
     rs <- forM [0..(n - 1)] $ \i -> newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return TimingStatsViewState { timingStatsView       = view,
                                   timingStatsExperiment = exp,
                                   timingStatsMap        = m }
       
-- | Get the timing statistics during the simulation.
simulateTimingStats :: TimingStatsViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateTimingStats st expdata =
  do let labels = timingStatsSeries $ timingStatsView st
         input providers =
           flip map providers $ \provider ->
           case providerToDouble provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as double values: simulateTimingStats"
             Just input -> (provider, input)
     i <- liftSimulation simulationIndex
     let r = fromJust $ M.lookup (i - 1) $ timingStatsMap st
     t <- time
     forM_ labels $ \label ->
       do let providers = experimentSeriesProviders expdata [label]
              pairs     = input providers
          forM_ pairs $ \(provider, input) ->
            do stats <- liftIO $ newIORef emptyTimingStats
               let name = providerName provider
               liftIO $ modifyIORef r ((:) (name, stats))
               enqueue (experimentQueue expdata) t $
                 -- we must subscribe through the event queue;
                 -- otherwise, we will loose a signal in the start time,
                 -- because the handleSignal_ function checks the event queue
                 handleSignal_ (experimentMixedSignal expdata [provider]) $ \_ ->
                 do t <- time
                    x <- input
                    liftIO $ modifyIORef stats $ addTimingStats t x
     return $ return ()
     
-- | Get the HTML code.     
timingStatsHtml :: TimingStatsViewState -> Int -> HtmlWriter ()     
timingStatsHtml st index =
  let n = experimentRunCount $ timingStatsExperiment st
  in if n == 1
     then timingStatsHtmlSingle st index
     else timingStatsHtmlMultiple st index
     
-- | Get the HTML code for a single run.
timingStatsHtmlSingle :: TimingStatsViewState -> Int -> HtmlWriter ()
timingStatsHtmlSingle st index =
  do header st index
     let r = fromJust $ M.lookup 0 (timingStatsMap st)
     pairs <- liftIO $ readIORef r
     forM_ (reverse pairs) $ \(name, r) ->
       do stats <- liftIO $ readIORef r
          let writer = timingStatsWriter (timingStatsView st)
              write  = timingStatsWrite writer
          write writer name stats

-- | Get the HTML code for multiple runs
timingStatsHtmlMultiple :: TimingStatsViewState -> Int -> HtmlWriter ()
timingStatsHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ timingStatsExperiment st
     forM_ [0..(n - 1)] $ \i ->
       do let subtitle = 
                replace "$RUN_INDEX" (show $ i + 1) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (timingStatsTitle $ timingStatsView st)
                (timingStatsRunTitle $ timingStatsView st)
          writeHtmlHeader4 $
            writeHtmlText subtitle
          let r = fromJust $ M.lookup i (timingStatsMap st)
          pairs <- liftIO $ readIORef r
          forM_ (reverse pairs) $ \(name, r) ->
            do stats <- liftIO $ readIORef r
               let writer = timingStatsWriter (timingStatsView st)
                   write  = timingStatsWrite writer
               write writer name stats

header :: TimingStatsViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (timingStatsTitle $ timingStatsView st)
     let description = timingStatsDescription $ timingStatsView st
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

-- | Get the TOC item     
timingStatsTOCHtml :: TimingStatsViewState -> Int -> HtmlWriter ()
timingStatsTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (timingStatsTitle $ timingStatsView st)