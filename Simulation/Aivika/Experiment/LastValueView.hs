
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.LastValueView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'LastValueView' that shows the last values
-- for the simulation variables.
--

module Simulation.Aivika.Experiment.LastValueView 
       (LastValueView(..),
        defaultLastValueView) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.IORef
import Data.Maybe

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.Utils (replace)

import Simulation.Aivika.Specs
import Simulation.Aivika.Parameter
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Signal

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
  
-- | This is the default view.
defaultLastValueView :: LastValueView
defaultLastValueView =  
  LastValueView { lastValueTitle       = "The Last Values",
                  lastValueRunTitle    = "$TITLE / Run $RUN_INDEX of $RUN_COUNT",
                  lastValueDescription = "It shows the values in the final time points.",
                  lastValueFormatter   = id,
                  lastValueSeries      = [] }
  
instance ExperimentView LastValueView r where  
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newLastValues v exp
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateLastValues st,
                                         reporterTOCHtml    = lastValueTOCHtml st,
                                         reporterHtml       = lastValueHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data LastValueViewState r =
  LastValueViewState { lastValueView       :: LastValueView,
                       lastValueExperiment :: Experiment r,
                       lastValueMap        :: M.Map Int (IORef [(String, String)]) }
  
-- | Create a new state of the view.
newLastValues :: LastValueView -> Experiment r -> IO (LastValueViewState r)
newLastValues view exp =
  do let n = experimentRunCount exp
     rs <- forM [0..(n - 1)] $ \i -> newIORef []    
     let m = M.fromList $ zip [0..(n - 1)] rs
     return LastValueViewState { lastValueView       = view,
                                 lastValueExperiment = exp,
                                 lastValueMap        = m }
       
-- | Get the last values during the simulation.
simulateLastValues :: LastValueViewState r -> ExperimentData -> Event (Event ())
simulateLastValues st expdata =
  do let labels = lastValueSeries $ lastValueView st
         input  =
           flip map (experimentSeriesProviders expdata labels) $ \provider ->
           case providerToString provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as a string: simulateLastValues"
             Just input -> (providerName provider, input)
     i <- liftParameter simulationIndex
     handleSignal_ (experimentSignalInStopTime expdata) $ \t ->
       do let r = fromJust $ M.lookup (i - 1) (lastValueMap st)
          output <- forM input $ \(name, input) ->
            do x <- input
               return (name, x)
          liftIO $ writeIORef r output
     return $ return ()
     
-- | Get the HTML code.     
lastValueHtml :: LastValueViewState r -> Int -> HtmlWriter ()     
lastValueHtml st index =
  let n = experimentRunCount $ lastValueExperiment st
  in if n == 1
     then lastValueHtmlSingle st index
     else lastValueHtmlMultiple st index
     
-- | Get the HTML code for a single run.
lastValueHtmlSingle :: LastValueViewState r -> Int -> HtmlWriter ()
lastValueHtmlSingle st index =
  do header st index
     let r = fromJust $ M.lookup 0 (lastValueMap st)
     pairs <- liftIO $ readIORef r
     forM_ pairs $ \pair ->
       formatPair pair (lastValueFormatter $ lastValueView st)

-- | Get the HTML code for multiple runs
lastValueHtmlMultiple :: LastValueViewState r -> Int -> HtmlWriter ()
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

header :: LastValueViewState r -> Int -> HtmlWriter ()
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
lastValueTOCHtml :: LastValueViewState r -> Int -> HtmlWriter ()
lastValueTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (lastValueTitle $ lastValueView st)
