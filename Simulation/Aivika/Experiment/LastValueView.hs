
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
       (LastValueView,
        outputLastValues,
        setLastValueTitle,
        getLastValueTitle,
        setLastValueRunTitle,
        getLastValueRunTitle,
        setLastValueDescription,
        getLastValueDescription,
        setLastValueFormatter,
        getLastValueFormatter,
        addLastValueSeries,
        setLastValueSeries,
        getLastValueSeries) where

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
  LastValueView { lvalTitle       :: String,
                  lvalRunTitle    :: String,
                  lvalDescription :: String,
                  lvalFormatter   :: ShowS,
                  lvalSeries      :: [String] }
  
instance View LastValueView where
  
  initView = 
    LastValueView { lvalTitle       = "The Last Values",
                    lvalRunTitle    = "$TITLE / $RUN_INDEX of $RUN_COUNT",
                    lvalDescription = [],
                    lvalFormatter   = id,
                    lvalSeries      = [] }
  
  viewGenerator v = 
    let reporter task filepath =
          do st <- newLastValues v task
             return Reporter { reporterInitialise = return (),
                               reporterFinalise   = return (),
                               reporterSimulate   = simulateLastValues st,
                               reporterTOCHtml    = lastValueTOCHtml st,
                               reporterHtml       = lastValueHtml st }
    in Generator { generateReporter = reporter }
  
-- | Output the last values.
outputLastValues :: State LastValueView () -> Experiment ()
outputLastValues = outputView
  
-- | Set the title for the Last Value view.
setLastValueTitle :: String -> State LastValueView ()
setLastValueTitle title =
  state (\lval -> ((), lval { lvalTitle = title }))
  
-- | Set the run title for the Last Value view. It may include
-- special variables @$RUN_INDEX@, @$RUN_COUNT@ and @$TITLE@.
setLastValueRunTitle :: String -> State LastValueView ()
setLastValueRunTitle title =
  state (\lval -> ((), lval { lvalRunTitle = title }))
  
-- | Set the description for the Last Value view.
setLastValueDescription :: String -> State LastValueView ()
setLastValueDescription description =
  state (\lval -> ((), lval { lvalDescription = description }))
  
-- | Set the formatter for the Last Value view.
setLastValueFormatter :: ShowS -> State LastValueView ()
setLastValueFormatter formatter =
  state (\lval -> ((), lval { lvalFormatter = formatter }))
  
-- | Add a new series with the specified label to the view.
addLastValueSeries :: String -> State LastValueView ()
addLastValueSeries label =
  state (\lval -> ((), lval { lvalSeries = label : lvalSeries lval }))
  
-- | Set the series with the specified label for the view.
setLastValueSeries :: [String] -> State LastValueView ()
setLastValueSeries labels =
  state (\lval -> ((), lval { lvalSeries = labels }))
  
-- | Get the title for the Last Value view.
getLastValueTitle :: State LastValueView String
getLastValueTitle =
  state (\lval -> (lvalTitle lval, lval))
  
-- | Get the run title for the Last Value view.
getLastValueRunTitle :: State LastValueView String
getLastValueRunTitle =
  state (\lval -> (lvalRunTitle lval, lval))
  
-- | Get the description for the Last Value view.
getLastValueDescription :: State LastValueView String
getLastValueDescription =
  state (\lval -> (lvalDescription lval, lval))
  
-- | Get the formatter for the Last Value view.
getLastValueFormatter :: State LastValueView ShowS
getLastValueFormatter =
  state (\lval -> (lvalFormatter lval, lval))
  
-- | Get the series for the Last Value view.
getLastValueSeries :: State LastValueView [String]
getLastValueSeries =
  state (\lval -> (reverse $ lvalSeries lval, lval))
  
-- | The state of the Last Value view.
data LastValueViewState =
  LastValueViewState { lvalView  :: LastValueView,
                       lvalTask  :: Task,
                       lvalMap   :: M.Map Int (IORef [(String, String)]) }
  
-- | Create a new state of the Last Value view.
newLastValues :: LastValueView -> Task -> IO LastValueViewState
newLastValues view task =
  do let n = taskRunCount task
     rs <- forM [0..(n - 1)] $ \i -> newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return LastValueViewState { lvalView = view,
                                 lvalTask = task,
                                 lvalMap  = m }
       
-- | Get the last values during the simulation.
simulateLastValues :: LastValueViewState -> ExperimentData -> Dynamics (Dynamics ())
simulateLastValues st expdata =
  do let labels = reverse $ lvalSeries $ lvalView st
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
       do let r = fromJust $ M.lookup (i - 1) (lvalMap st)
          output <- forM input $ \(name, input) ->
            do x <- input
               return (name, x)
          liftIO $ writeIORef r output
     
-- | Get the HTML code.     
lastValueHtml :: LastValueViewState -> Int -> HtmlWriter ()     
lastValueHtml st index =
  let n = taskRunCount $ lvalTask st
  in if n == 1
     then lastValueHtmlSingle st index
     else lastValueHtmlMultiple st index
     
-- | Get the HTML code for a single run.
lastValueHtmlSingle :: LastValueViewState -> Int -> HtmlWriter ()
lastValueHtmlSingle st index =
  do header st index
     let r = fromJust $ M.lookup 0 (lvalMap st)
     pairs <- liftIO $ readIORef r
     forM_ pairs $ \pair ->
       formatPair pair (lvalFormatter $ lvalView st)

-- | Get the HTML code for multiple runs
lastValueHtmlMultiple :: LastValueViewState -> Int -> HtmlWriter ()
lastValueHtmlMultiple st index =
  do header st index
     let n = taskRunCount $ lvalTask st
     forM_ [0..(n - 1)] $ \i ->
       do let subtitle = 
                replace "$RUN_INDEX" (show $ i + 1) $
                replace "$RUN_COUNT" (show n) $
                replace "$TITLE" (lvalTitle $ lvalView st) $
                (lvalRunTitle $ lvalView st)
          writeHtml "<h4>"
          writeHtmlText subtitle
          writeHtml "</h4>"
          let r = fromJust $ M.lookup i (lvalMap st)
          pairs <- liftIO $ readIORef r
          forM_ pairs $ \pair ->
            formatPair pair (lvalFormatter $ lvalView st)

header :: LastValueViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtml "<h3 id=\"id"
     writeHtml $ show index
     writeHtml "\">"
     writeHtmlText $ (lvalTitle $ lvalView st)
     writeHtml "</h3>"
     let description = (lvalDescription $ lvalView st)
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
     writeHtmlText $ (lvalTitle $ lvalView st)
     writeHtml "</a></h4>"