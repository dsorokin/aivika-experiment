
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Module     : Simulation.Aivika.Experiment.ExperimentSpecsView
-- Copyright  : Copyright (c) 2012, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.4.1
--
-- The module defines 'ExperimentSpecsView' that shows the 
-- experiment specs.
--

module Simulation.Aivika.Experiment.ExperimentSpecsView 
       (ExperimentSpecsView(..),
        defaultExperimentSpecsView) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.ExperimentSpecsWriter

-- | Defines the 'View' that shows the experiment specs.
data ExperimentSpecsView r =
  ExperimentSpecsView { experimentSpecsTitle       :: String,
                        -- ^ The title for the view.
                        experimentSpecsDescription :: String,
                        -- ^ The description for the view.
                        experimentSpecsWriter      :: ExperimentSpecsWriter r
                        -- ^ It shows the specs.
                      }
  
-- | This is the default view.
defaultExperimentSpecsView :: ExperimentSpecsView r
defaultExperimentSpecsView =  
  ExperimentSpecsView { experimentSpecsTitle       = "Experiment Specs",
                        experimentSpecsDescription = "It shows the experiment specs.",
                        experimentSpecsWriter      = defaultExperimentSpecsWriter }

instance ExperimentView (ExperimentSpecsView r) r where  
  
  outputView v = 
    let reporter exp renderer dir =
          do st <- newExperimentSpecs v exp
             return ExperimentReporter { reporterInitialise = return (),
                                         reporterFinalise   = return (),
                                         reporterSimulate   = const $ return $ return (),
                                         reporterTOCHtml    = experimentSpecsTOCHtml st,
                                         reporterHtml       = experimentSpecsHtml st }
    in ExperimentGenerator { generateReporter = reporter }
  
-- | The state of the view.
data ExperimentSpecsViewState r =
  ExperimentSpecsViewState { experimentSpecsView       :: ExperimentSpecsView r,
                             experimentSpecsExperiment :: Experiment r }
  
-- | Create a new state of the view.
newExperimentSpecs :: ExperimentSpecsView r -> Experiment r -> IO (ExperimentSpecsViewState r)
newExperimentSpecs view exp =
  return ExperimentSpecsViewState { experimentSpecsView       = view,
                                    experimentSpecsExperiment = exp }
       
-- | Get the HTML code.     
experimentSpecsHtml :: ExperimentSpecsViewState r -> Int -> HtmlWriter ()     
experimentSpecsHtml st index =
  do header st index
     let writer = experimentSpecsWriter (experimentSpecsView st)
         write  = experimentSpecsWrite writer
         exp    = experimentSpecsExperiment st
     write writer exp

header :: ExperimentSpecsViewState r -> Int -> HtmlWriter ()
header st index =
  do writeHtmlHeader3WithId ("id" ++ show index) $
       writeHtmlText (experimentSpecsTitle $ experimentSpecsView st)
     let description = experimentSpecsDescription $ experimentSpecsView st
     unless (null description) $
       writeHtmlParagraph $
       writeHtmlText description

-- | Get the TOC item     
experimentSpecsTOCHtml :: ExperimentSpecsViewState r -> Int -> HtmlWriter ()
experimentSpecsTOCHtml st index =
  writeHtmlListItem $
  writeHtmlLink ("#id" ++ show index) $
  writeHtmlText (experimentSpecsTitle $ experimentSpecsView st)
