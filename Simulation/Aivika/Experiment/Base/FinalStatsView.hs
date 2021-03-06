
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Base.FinalStatsView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'FinalStatsView' that gathers the statistics
-- in the final time points for different simulation runs.
--

module Simulation.Aivika.Experiment.Base.FinalStatsView
       (FinalStatsView(..), 
        defaultFinalStatsView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.IORef
import Data.Maybe

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.WebPageRenderer
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.SamplingStatsWriter
import Simulation.Aivika.Experiment.Concurrent.MVar

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
                   finalStatsTransform   :: ResultTransform,
                   -- ^ The transform applied to the results before receiving series.
                   finalStatsSeries      :: ResultTransform 
                   -- ^ It defines the series for which the statistics to be collected.
                 }
  
-- | The default statistics view.  
defaultFinalStatsView :: FinalStatsView
defaultFinalStatsView = 
  FinalStatsView { finalStatsTitle       = "Final Statistics Based on Observations",
                   finalStatsDescription = "Statistics is gathered in final time points for all runs.",
                   finalStatsWriter      = defaultSamplingStatsWriter,
                   finalStatsPredicate   = return True,
                   finalStatsTransform   = id,
                   finalStatsSeries      = id }

instance ExperimentView FinalStatsView (WebPageRenderer a) where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newFinalStats v exp dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = finalStatsTOCHtml st,
                                   reporterWriteHtml    = finalStatsHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateFinalStats st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data FinalStatsViewState =
  FinalStatsViewState { finalStatsView       :: FinalStatsView,
                        finalStatsExperiment :: Experiment,
                        finalStatsResults    :: MVar (Maybe FinalStatsResults) }

-- | The statistics results.
data FinalStatsResults =
  FinalStatsResults { finalStatsNames  :: [String],
                      finalStatsValues :: [MVar (SamplingStats Double)] }
  
-- | Create a new state of the view.
newFinalStats :: FinalStatsView -> Experiment -> FilePath -> ExperimentWriter FinalStatsViewState
newFinalStats view exp dir =
  do r <- liftIO $ newMVar Nothing
     return FinalStatsViewState { finalStatsView       = view,
                                  finalStatsExperiment = exp,
                                  finalStatsResults    = r }
       
-- | Create new statistics results.
newFinalStatsResults :: [String] -> Experiment -> IO FinalStatsResults
newFinalStatsResults names exp =
  do values <- forM names $ \_ -> liftIO $ newMVar emptySamplingStats
     return FinalStatsResults { finalStatsNames  = names,
                                finalStatsValues = values }

-- | Require to return unique final statistics results associated with the specified state. 
requireFinalStatsResults :: FinalStatsViewState -> [String] -> IO FinalStatsResults
requireFinalStatsResults st names =
  maybePutMVar (finalStatsResults st)
  (newFinalStatsResults names (finalStatsExperiment st)) $ \results ->
  if (names /= finalStatsNames results)
  then error "Series with different names are returned for different runs: requireFinalStatsResults"
  else return results
       
-- | Simulate the specified series.
simulateFinalStats :: FinalStatsViewState -> ExperimentData -> Composite ()
simulateFinalStats st expdata =
  do let view    = finalStatsView st
         rs      = finalStatsSeries view $
                   finalStatsTransform view $
                   experimentResults expdata
         loc     = localisePathResultTitle $
                   experimentLocalisation $
                   finalStatsExperiment st
         exts    = resultsToDoubleStatsEitherValues rs
         signals = experimentPredefinedSignals expdata
         signal  = filterSignalM (const predicate) $
                   resultSignalInStopTime signals
         names   = map (loc . resultValueIdPath) exts
         predicate = finalStatsPredicate view
     results <- liftIO $ requireFinalStatsResults st names
     let values = finalStatsValues results 
     handleSignalComposite signal $ \_ ->
       forM_ (zip exts values) $ \(ext, value) ->
       do x <- resultValueData ext
          liftIO $
            modifyMVar_ value $ \y ->
            let y' = combineSamplingStatsEither x y
            in y' `seq` return y'

-- | Get the HTML code.     
finalStatsHtml :: FinalStatsViewState -> Int -> HtmlWriter ()
finalStatsHtml st index =
  do header st index
     results <- liftIO $ readMVar (finalStatsResults st)
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = finalStatsNames results
                values = finalStatsValues results
                writer = finalStatsWriter (finalStatsView st)
                write  = samplingStatsWrite writer
            forM_ (zip names values) $ \(name, value) ->
              do stats <- liftIO $ readMVar value
                 write writer name stats

header :: FinalStatsViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (finalStatsTitle $ finalStatsView st)
     let description = finalStatsDescription $ finalStatsView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
finalStatsTOCHtml :: FinalStatsViewState -> Int -> HtmlWriter ()
finalStatsTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (finalStatsTitle $ finalStatsView st)
