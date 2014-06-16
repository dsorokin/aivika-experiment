
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.FinalStatsView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'FinalStatsView' gathers the statistics
-- in the final time points for different simulation runs.
--

module Simulation.Aivika.Experiment.FinalStatsView
       (FinalStatsView(..), 
        defaultFinalStatsView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.IORef
import Data.Maybe

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.SamplingStatsWriter
import Simulation.Aivika.Experiment.SamplingStatsSource

import Simulation.Aivika.Specs
import Simulation.Aivika.Simulation
import Simulation.Aivika.Event
import Simulation.Aivika.Signal
import Simulation.Aivika.Statistics

-- | Defines the 'View' that gathers the statistics
-- in the final time points.
data FinalStatsView =
  FinalStatsView { finalStatsTitle       :: String,
                   -- ^ This is a title for the view.
                   finalStatsDescription :: String,
                   -- ^ This is a description used in HTML.
                   finalStatsWriter      :: SamplingStatsWriter Double,
                   -- ^ It shows the sampling statistics.
                   finalStatsPredicate   :: Event Bool,
                   -- ^ It specifies the predicate that defines
                   -- when we count data when gathering the statistics.
                   finalStatsSeries      :: [String]
                   -- ^ It contains the labels of data for which
                   -- the statistics is collected.
                 }
  
-- | The default statistics view.  
defaultFinalStatsView :: FinalStatsView
defaultFinalStatsView = 
  FinalStatsView { finalStatsTitle       = "Final Statistics",
                   finalStatsDescription = "The statistical data are gathered in the final time points for all runs.",
                   finalStatsWriter      = defaultSamplingStatsWriter,
                   finalStatsPredicate   = return True,
                   finalStatsSeries      = [] }

instance ExperimentView FinalStatsView r where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newFinalStats v exp dir
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateFinalStats st,
                                         reporterTOCHtml    = finalStatsTOCHtml st,
                                         reporterHtml       = finalStatsHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalStatsViewState r =
  FinalStatsViewState { finalStatsView       :: FinalStatsView,
                        finalStatsExperiment :: Experiment r,
                        finalStatsLock       :: MVar (),
                        finalStatsResults    :: IORef (Maybe FinalStatsResults) }

-- | The statistics results.
data FinalStatsResults =
  FinalStatsResults { finalStatsNames  :: [String],
                      finalStatsValues :: [IORef (SamplingStats Double)] }
  
-- | Create a new state of the view.
newFinalStats :: FinalStatsView -> Experiment r -> FilePath -> IO (FinalStatsViewState r)
newFinalStats view exp dir =
  do l <- newMVar () 
     r <- newIORef Nothing
     return FinalStatsViewState { finalStatsView       = view,
                                  finalStatsExperiment = exp,
                                  finalStatsLock       = l, 
                                  finalStatsResults    = r }
       
-- | Create new statistics results.
newFinalStatsResults :: [String] -> Experiment r -> IO FinalStatsResults
newFinalStatsResults names exp =
  do values <- forM names $ \_ -> liftIO $ newIORef emptySamplingStats
     return FinalStatsResults { finalStatsNames  = names,
                                finalStatsValues = values }
       
-- | Simulation the specified series.
simulateFinalStats :: FinalStatsViewState r -> ExperimentData -> Event (Event ())
simulateFinalStats st expdata =
  do let protolabels = finalStatsSeries $ finalStatsView st
         protoproviders = flip map protolabels $ \protolabel ->
           experimentSeriesProviders expdata [protolabel]
         providers = concat protoproviders
         input =
           flip map providers $ \provider ->
           case providerToDoubleStatsSource provider of
             Nothing -> error $
                        "Cannot represent series " ++
                        providerName provider ++ 
                        " as a source of double values: simulateFinalStats"
             Just input -> samplingStatsSourceData input
         names = map providerName providers
         predicate = finalStatsPredicate $ finalStatsView st
         exp = finalStatsExperiment st
         lock = finalStatsLock st
     results <- liftIO $ readIORef (finalStatsResults st)
     case results of
       Nothing ->
         liftIO $
         do results <- newFinalStatsResults names exp
            writeIORef (finalStatsResults st) $ Just results
       Just results ->
         when (names /= finalStatsNames results) $
         error "Series with different names are returned for different runs: simulateFinalStats"
     results <- liftIO $ fmap fromJust $ readIORef (finalStatsResults st)
     let values = finalStatsValues results
         h = filterSignalM (const predicate) $
             experimentSignalInStopTime expdata
     handleSignal_ h $ \_ ->
       do xs <- sequence input
          liftIO $ withMVar lock $ \() ->
            forM_ (zip xs values) $ \(x, values) ->
            do y <- readIORef values
               let y' = addDataToSamplingStats x y
               y' `seq` writeIORef values y'
     return $ return ()

-- | Get the HTML code.     
finalStatsHtml :: FinalStatsViewState r -> Int -> HtmlWriter ()
finalStatsHtml st index =
  do header st index
     results <- liftIO $ readIORef (finalStatsResults st)
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalStatsNames results
                values = finalStatsValues results
                writer = finalStatsWriter (finalStatsView st)
                write  = samplingStatsWrite writer
            forM_ (zip names values) $ \(name, value) ->
              do stats <- liftIO $ readIORef value
                 write writer name stats

header :: FinalStatsViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalStatsTitle $ finalStatsView st)
     let description = finalStatsDescription $ finalStatsView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalStatsTOCHtml :: FinalStatsViewState r -> Int -> HtmlWriter ()
finalStatsTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalStatsTitle $ finalStatsView st)
