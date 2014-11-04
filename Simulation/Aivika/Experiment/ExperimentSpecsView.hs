
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.ExperimentSpecsView
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- The module defines 'ExperimentSpecsView' that shows the 
-- experiment specs.
--

module Simulation.Aivika.Experiment.ExperimentSpecsView 
       (ExperimentSpecsView(..),
        defaultExperimentSpecsView) where

import Control.Monad
import Control.Monad.Trans

import Data.Monoid

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.WebPageRenderer
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.ExperimentWriter
import Simulation.Aivika.Experiment.ExperimentSpecsWriter

-- | Defines the 'View' that shows the experiment specs.
data ExperimentSpecsView =
  ExperimentSpecsView { experimentSpecsTitle       :: String,
                        -- ^ The title for the view.
                        experimentSpecsDescription :: String,
                        -- ^ The description for the view.
                        experimentSpecsWriter      :: ExperimentSpecsWriter
                        -- ^ It shows the specs.
                      }
  
-- | This is the default view.
defaultExperimentSpecsView :: ExperimentSpecsView
defaultExperimentSpecsView =  
  ExperimentSpecsView { experimentSpecsTitle       = "Experiment Specs",
                        experimentSpecsDescription = "It shows the experiment specs.",
                        experimentSpecsWriter      = defaultExperimentSpecsWriter }

instance ExperimentView ExperimentSpecsView (WebPageRenderer a) where  
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newExperimentSpecs v exp
             let context =
                   WebPageContext $
                   WebPageWriter { reporterWriteTOCHtml = experimentSpecsTOCHtml st,
                                   reporterWriteHtml    = experimentSpecsHtml st }
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = const $ return mempty,
                                         reporterContext    = context }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data ExperimentSpecsViewState =
  ExperimentSpecsViewState { experimentSpecsView       :: ExperimentSpecsView,
                             experimentSpecsExperiment :: Experiment }
  
-- | Create a new state of the view.
newExperimentSpecs :: ExperimentSpecsView -> Experiment -> ExperimentWriter ExperimentSpecsViewState
newExperimentSpecs view exp =
  return ExperimentSpecsViewState { experimentSpecsView       = view,
                                    experimentSpecsExperiment = exp }
       
-- | Get the HTML code.     
experimentSpecsHtml :: ExperimentSpecsViewState -> Int -> HtmlWriter ()     
experimentSpecsHtml st index =
  do header st index
     let writer = experimentSpecsWriter (experimentSpecsView st)
         write  = experimentSpecsWrite writer
         exp    = experimentSpecsExperiment st
     write writer exp

header :: ExperimentSpecsViewState -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (experimentSpecsTitle $ experimentSpecsView st)
     let description = experimentSpecsDescription $ experimentSpecsView st
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

-- | Get the TOC item     
experimentSpecsTOCHtml :: ExperimentSpecsViewState -> Int -> HtmlWriter ()
experimentSpecsTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (experimentSpecsTitle $ experimentSpecsView st)
