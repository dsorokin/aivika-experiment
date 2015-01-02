
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.TimingStatsView
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
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
import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.WebPageRenderer
import Simulation.Aivika.Experiment.ExperimentWriter
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.TimingStatsWriter
import Simulation.Aivika.Experiment.Utils (replace)

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
                    timingStatsPredicate   :: Event Bool,
                    -- ^ Specifies when gathering the statistics.
                    timingStatsTransform   :: ResultTransform,
                    -- ^ The transform applied to the results before receiving series.
                    timingStatsSeries      :: ResultTransform 
                    -- ^ It defines the series for which the statistics to be collected.
                  }
  
-- | This is the default view.
defaultTimingStatsView :: TimingStatsView
defaultTimingStatsView =  
  TimingStatsView { timingStatsTitle       = "Timing Statistics",
                    timingStatsRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                    timingStatsDescription = "The statistical data are gathered in the time points.",
                    timingStatsWriter      = defaultTimingStatsWriter,
                    timingStatsPredicate   = return True,
                    timingStatsTransform   = id,
                    timingStatsSeries      = id }

instance ExperimentView TimingStatsView (WebPageRenderer a) where  
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newTimingStats v exp
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = timingStatsTOCHtml st,
                                   reporterWriteHtml    = timingStatsHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateTimingStats st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data TimingStatsViewState =
  TimingStatsViewState { timingStatsView       :: TimingStatsView,
                         timingStatsExperiment :: Experiment,
                         timingStatsMap        :: M.Map Int (IORef [(String, IORef (TimingStats Double))]) }
  
-- | Create a new state of the view.
newTimingStats :: TimingStatsView -> Experiment -> ExperimentWriter TimingStatsViewState
newTimingStats view exp =
  do let n = experimentRunCount exp
     rs <- forM [0..(n - 1)] $ \i -> liftIO $ newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return TimingStatsViewState { timingStatsView       = view,
                                   timingStatsExperiment = exp,
                                   timingStatsMap        = m }
       
-- | Get the timing statistics during the simulation.
simulateTimingStats :: TimingStatsViewState -> ExperimentData -> Event DisposableEvent
simulateTimingStats st expdata =
  do let view    = timingStatsView st
         rs      = timingStatsSeries view $
                   timingStatsTransform view $
                   experimentResults expdata
         exts    = resultsToDoubleValues rs
         signals = experimentPredefinedSignals expdata
         signal  = filterSignalM (const predicate) $
                   pureResultSignal signals $
                   resultSignal rs
         predicate = timingStatsPredicate view
     i <- liftParameter simulationIndex
     let r = fromJust $ M.lookup (i - 1) $ timingStatsMap st
     ds <- forM exts $ \ext ->
       do stats <- liftIO $ newIORef emptyTimingStats
          let name = resultValueName ext
          liftIO $ modifyIORef r ((:) (name, stats))
          handleSignal signal $ \_ ->
            do t <- liftDynamics time
               x <- resultValueData ext
               liftIO $
                 do y <- readIORef stats
                    let y' = addTimingStats t x y
                    y' `seq` writeIORef stats y'
     return $ mconcat ds
     
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
