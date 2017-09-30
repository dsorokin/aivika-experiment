
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Base.InfoView
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines 'InfoView' that shows the description of series.
--

module Simulation.Aivika.Experiment.Base.InfoView
       (InfoView(..), 
        defaultInfoView) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.IORef
import Data.Maybe
import Data.Monoid

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.WebPageRenderer
import Simulation.Aivika.Experiment.Base.ExperimentWriter
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Concurrent.MVar

-- | Defines the 'View' that shows the description of series.
data InfoView =
  InfoView { infoTitle        :: String,
             -- ^ This is a title for the view.
             infoDescription  :: String,
             -- ^ This is a text description used in HTML.
             infoTransform    :: ResultTransform,
             -- ^ The transform applied to the results before receiving series.
             infoSeries       :: ResultTransform
             -- ^ It defines the series for which the description is shown.
           }
  
-- | The default description view.  
defaultInfoView :: InfoView
defaultInfoView = 
  InfoView { infoTitle        = "Information",
             infoDescription  = "It shows the information about simulation entities:",
             infoTransform    = id,
             infoSeries       = id }

instance ExperimentView InfoView (WebPageRenderer a) where
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newInfo v exp dir
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = infoTOCHtml st,
                                   reporterWriteHtml    = infoHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = simulateInfo st,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data InfoViewState =
  InfoViewState { infoView       :: InfoView,
                  infoExperiment :: Experiment,
                  infoResults    :: MVar (Maybe InfoResults) }

-- | The information table.
data InfoResults =
  InfoResults { infoNames  :: [String],
                infoValues :: [String] }
  
-- | Create a new state of the view.
newInfo :: InfoView -> Experiment -> FilePath -> ExperimentWriter InfoViewState
newInfo view exp dir =
  do r <- liftIO $ newMVar Nothing
     return InfoViewState { infoView       = view,
                            infoExperiment = exp,
                            infoResults    = r }
       
-- | Create a new information table.
newInfoResults :: [ResultSource] -> ResultLocalisation -> Experiment -> IO InfoResults
newInfoResults sources loc exp =
  do let xs =
           flip map sources $ \source ->
           case source of
             ResultItemSource (ResultItem x) ->
               [(resultNameToTitle $ resultItemName x,
                 localiseResultDescription loc $ resultItemId x)]
             ResultObjectSource x ->
               [(resultNameToTitle $ resultObjectName x,
                 localiseResultDescription loc $ resultObjectId x)]
             ResultVectorSource x ->
               [(resultNameToTitle $ resultVectorName x,
                 localiseResultDescription loc $ resultVectorId x)]
             ResultSeparatorSource x ->
               []
         (names, values) = unzip $ concat xs
     return InfoResults { infoNames  = names,
                          infoValues = values }

-- | Require to return the unique information table associated with the specified state. 
requireInfoResults :: InfoViewState -> [ResultSource] -> IO InfoResults
requireInfoResults st sources =
  let view = infoView st
      exp  = infoExperiment st
      loc  = experimentLocalisation exp
  in maybePutMVar (infoResults st)
     (newInfoResults sources loc exp) $ \results ->
  do let xs =
           flip map sources $ \source ->
           case source of
             ResultItemSource (ResultItem x) ->
               [resultNameToTitle $ resultItemName x]
             ResultObjectSource x ->
               [resultNameToTitle $ resultObjectName x]
             ResultVectorSource x ->
               [resultNameToTitle $ resultVectorName x]
             ResultSeparatorSource x ->
               []
     let names = concat xs
     if (names /= infoNames results)
       then error "Series with different names are returned for different runs: requireInfoResults"
       else return results
       
-- | Simulate the specified series.
simulateInfo :: InfoViewState -> ExperimentData -> Composite ()
simulateInfo st expdata =
  do let view    = infoView st
         rs      = infoSeries view $
                   infoTransform view $
                   experimentResults expdata
         sources = resultSourceList rs
     liftIO $ requireInfoResults st sources
     return ()

-- | Get the HTML code.     
infoHtml :: InfoViewState -> Int -> HtmlWriter ()
infoHtml st index =
  do header st index
     results <- liftIO $ readMVar (infoResults st)
     case results of
       Nothing -> return ()
       Just results ->
         do let names  = infoNames results
                values = infoValues results
            writeHtmlList $
              forM_ (zip names values) $ \(name, value) ->
              writeHtmlListItem $
              do writeHtmlText name
                 writeHtmlText " - "
                 writeHtmlText value

header :: InfoViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $ 
       writeHtmlText (infoTitle $ infoView st)
     let description = infoDescription $ infoView st
     unless (null description) $
       writeHtmlParagraph $ 
       writeHtmlText description

-- | Get the TOC item.
infoTOCHtml :: InfoViewState -> Int -> HtmlWriter ()
infoTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (infoTitle $ infoView st)
