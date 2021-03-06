
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Base.LastValueView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'LastValueView' that shows the last values
-- for the simulation variables.
--

module Simulation.Aivika.Experiment.Base.LastValueView 
       (LastValueView(..),
        defaultLastValueView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.WebPageRenderer
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

-- | Defines the 'View' that shows the last values of the simulation
-- variables.
data LastValueView =
  LastValueView { lastValueTitle       :: String,
                  -- ^ The title for the view.
                  lastValueRunTitle    :: String,
                  -- ^ The run title for the view. It may include
                  -- special variables @$RUN_INDEX@, @$RUN_COUNT@ and 
                  -- @$TITLE@.
                  --
                  -- An example is 
                  --
                  -- @
                  --   lastValueRunTitle = \"$TITLE / Run $RUN_INDEX of $RUN_COUNT\"
                  -- @
                  lastValueDescription :: String,
                  -- ^ The description for the view.
                  lastValueFormatter   :: ShowS,
                  -- ^ It transforms data before they will be shown.
                  lastValueTransform   :: ResultTransform,
                  -- ^ The transform applied to the results before receiving series.
                  lastValueSeries      :: ResultTransform 
                  -- ^ It defines the series for which the last values to be shown.
                }
  
-- | This is the default view.
defaultLastValueView :: LastValueView
defaultLastValueView =  
  LastValueView { lastValueTitle       = "Last Values",
                  lastValueRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  lastValueDescription = "It shows the values in the final time point(s).",
                  lastValueFormatter   = id,
                  lastValueTransform   = id,
                  lastValueSeries      = id }
  
instance ExperimentView LastValueView (WebPageRenderer a) where  
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newLastValues v exp
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = lastValueTOCHtml st,
                                   reporterWriteHtml    = lastValueHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateLastValues st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }

-- | The state of the view.
data LastValueViewState =
  LastValueViewState { lastValueView       :: LastValueView,
                       lastValueExperiment :: Experiment,
                       lastValueMap        :: M.Map Int (IORef [(String, String)]) }
  
-- | Create a new state of the view.
newLastValues :: LastValueView -> Experiment -> ExperimentWriter LastValueViewState
newLastValues view exp =
  do let n = experimentRunCount exp
     rs <- forM [0..(n - 1)] $ \i -> liftIO $ newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return LastValueViewState { lastValueView       = view,
                                 lastValueExperiment = exp,
                                 lastValueMap        = m }
       
-- | Get the last values during the simulation.
simulateLastValues :: LastValueViewState -> ExperimentData -> Composite ()
simulateLastValues st expdata =
  do let view    = lastValueView st
         rs      = lastValueSeries view $
                   lastValueTransform view $
                   experimentResults expdata
         loc     = localisePathResultTitle $
                   experimentLocalisation $
                   lastValueExperiment st
         exts    = resultsToStringValues rs
         signals = experimentPredefinedSignals expdata
         signal  = resultSignalInStopTime signals
     i <- liftParameter simulationIndex
     handleSignalComposite signal $ \t ->
       do let r = fromJust $ M.lookup (i - 1) (lastValueMap st)
          output <- forM exts $ \ext ->
            do x <- resultValueData ext
               return (loc $ resultValueIdPath ext, x)
          liftIO $ writeIORef r output
     
-- | Get the HTML code.     
lastValueHtml :: LastValueViewState -> Int -> HtmlWriter ()     
lastValueHtml st index =
  let n = experimentRunCount $ lastValueExperiment st
  in if n == 1
     then lastValueHtmlSingle st index
     else lastValueHtmlMultiple st index
     
-- | Get the HTML code for a single run.
lastValueHtmlSingle :: LastValueViewState -> Int -> HtmlWriter ()
lastValueHtmlSingle st index =
  do header st index
     let r = fromJust $ M.lookup 0 (lastValueMap st)
     pairs <- liftIO $ readIORef r
     forM_ pairs $ \pair ->
       formatPair pair (lastValueFormatter $ lastValueView st)

-- | Get the HTML code for multiple runs
lastValueHtmlMultiple :: LastValueViewState -> Int -> HtmlWriter ()
lastValueHtmlMultiple st index =
  do header st index
     let n = experimentRunCount $ lastValueExperiment st
     forM_ [0..(n - 1)] $ \i ->
       do let subtitle = 
                replace "$RUN_INDEX" (show $ i + 1) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (lastValueTitle $ lastValueView st)
                (lastValueRunTitle $ lastValueView st)
          writeHtmlHeader4 $
            writeHtmlText subtitle
          let r = fromJust $ M.lookup i (lastValueMap st)
          pairs <- liftIO $ readIORef r
          forM_ pairs $ \pair ->
            formatPair pair (lastValueFormatter $ lastValueView st)

header :: LastValueViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (lastValueTitle $ lastValueView st)
     let description = lastValueDescription $ lastValueView st
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

formatPair :: (String, String) -> ShowS -> HtmlWriter ()
formatPair (name, value) formatter =
  writeHtmlParagraph $ 
  do writeHtmlText name
     writeHtmlText " = "
     writeHtmlText $ formatter value
          
-- | Get the TOC item     
lastValueTOCHtml :: LastValueViewState -> Int -> HtmlWriter ()
lastValueTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (lastValueTitle $ lastValueView st)
