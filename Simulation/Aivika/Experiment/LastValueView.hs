
-- |
-- Module     : Simulation.Aivika.Experiment.LastValueView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'LastValueView' that shows the last values
-- for the simulation variables.
--

module Simulation.Aivika.Experiment.LastValueView 
       (LastValueView(..),
        initLastValueView) where

import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import Data.String.Utils (replace)

import Simulation.Aivika.Experiment
import Simulation.Aivika.Experiment.HtmlWriter

import Simulation.Aivika.Dynamics
import Simulation.Aivika.Dynamics.Simulation
import Simulation.Aivika.Dynamics.Signal

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
                  lastValueSeries      :: [String] 
                  -- ^ It contains the labels of the observed series.
                }
  
-- | This is an initial view.
initLastValueView :: LastValueView
initLastValueView =  
  LastValueView { lastValueTitle       = "The Last Values",
                  lastValueRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  lastValueDescription = [],
                  lastValueFormatter   = id,
                  lastValueSeries      = [] }
  
instance View LastValueView where  
  
  outputView v = 
    let reporter exp dir =
          do st <- newLastValues v exp
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateLastValues st,
                               reporterTOCHtml    = lastValueTOCHtml st,
                               reporterHtml       = lastValueHtml st }
    in Generator { generateReporter = reporter }
  
-- | The state of the view.
data LastValueViewState =
  LastValueViewState { lastValueView       :: LastValueView,
                       lastValueExperiment :: Experiment,
                       lastValueMap        :: M.Map Int (IORef [(String, String)]) }
  
-- | Create a new state of the view.
newLastValues :: LastValueView -> Experiment -> IO LastValueViewState
newLastValues view exp =
  do let n = experimentRunCount exp
     rs <- forM [0..(n - 1)] $ \i -> newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return LastValueViewState { lastValueView       = view,
                                 lastValueExperiment = exp,
                                 lastValueMap        = m }
       
-- | Get the last values during the simulation.
simulateLastValues :: LastValueViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateLastValues st expdata =
  do let labels = lastValueSeries $ lastValueView st
         input  =
           flip map (experimentSeriesProviders expdata labels) $ \provider ->
           case providerToString provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        (providerName provider) ++ 
                        " as a string: simulateLastValues"
             Just input -> (providerName provider, input)
     i <- liftSimulation simulationIndex
     handleSignal (experimentSignalInStopTime expdata) $ \t ->
       do let r = fromJust $ M.lookup (i - 1) (lastValueMap st)
          output <- forM input $ \(name, input) ->
            do x <- input
               return (name, x)
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
                replace "$TITLE" (lastValueTitle $ lastValueView st) $
                (lastValueRunTitle $ lastValueView st)
          writeHtml "<h4>"
          writeHtmlText subtitle
          writeHtml "</h4>"
          let r = fromJust $ M.lookup i (lastValueMap st)
          pairs <- liftIO $ readIORef r
          forM_ pairs $ \pair ->
            formatPair pair (lastValueFormatter $ lastValueView st)

header :: LastValueViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtml "<h3 id=\"id"
     writeHtml $ show index
     writeHtml "\">"
     writeHtmlText $ (lastValueTitle $ lastValueView st)
     writeHtml "</h3>"
     let description = (lastValueDescription $ lastValueView st)
     unless (null description) $
       do writeHtml "<p>"
          writeHtmlText description
          writeHtml "</p>"

formatPair :: (String, String) -> ShowS -> HtmlWriter ()
formatPair (name, value) formatter =
  do writeHtml "<p>"
     writeHtmlText name
     writeHtmlText " = "
     writeHtmlText $ formatter value
     writeHtml "</p>"
          
-- | Get the TOC item     
lastValueTOCHtml :: LastValueViewState -> Int -> HtmlWriter ()
lastValueTOCHtml st index =
  do writeHtml "<h4><a href=\"#id"
     writeHtml $ show index
     writeHtml "\">"
     writeHtmlText $ (lastValueTitle $ lastValueView st)
     writeHtml "</a></h4>"