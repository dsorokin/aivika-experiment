
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a renderer that creates a web page when running the simulation experiment.
--

module Simulation.Aivika.Experiment.WebPageRenderer where

import Control.Monad
import Control.Monad.Trans

import qualified System.IO.UTF8 as UTF8
import System.Directory
import System.FilePath

import Simulation.Aivika
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.HtmlWriter
import Simulation.Aivika.Experiment.ExperimentWriter

-- | It defines the web page renderer for simulation 'Experiment'. 
data WebPageRenderer a = WebPageRenderer a
                         -- ^ A renderer that depends on the provided parameter.

-- | It replies to the requests made by the web page renderer.
data WebPageWriter =
  WebPageWriter { reporterWriteTOCHtml :: Int -> HtmlWriter (),
                  -- ^ Return a TOC (Table of Contents) item for 
                  -- the HTML index file after the finalisation 
                  -- function is called, i.e. in the very end. 
                  -- The agument specifies the ordered number of 
                  -- the item.
                  --
                  -- You should wrap your HTML in 'writeHtmlListItem'.
                  reporterWriteHtml :: Int -> HtmlWriter ()
                  -- ^ Return an HTML code for the index file
                  -- after the finalisation function is called,
                  -- i.e. in the very end. The agument specifies
                  -- the ordered number of the item.
                }

-- | A convenient type synonym for describing a web page generator.
type WebPageGenerator a = ExperimentGenerator (WebPageRenderer a)

-- | Rending a web page with results when running the simulation experiment.
instance ExperimentRendering (WebPageRenderer a) where

  -- | A web page context.
  newtype ExperimentContext (WebPageRenderer a) =
    WebPageContext { runWebPageContext :: WebPageWriter
                     -- ^ Run the web page context.
                   }

  renderExperiment e r reporters path = 
    do let html :: HtmlWriter ()
           html = 
             writeHtmlDocumentWithTitle (experimentTitle e) $
             do writeHtmlList $
                  forM_ (zip [1..] reporters) $ \(i, reporter) -> 
                  reporterWriteTOCHtml (runWebPageContext $
                                        reporterContext reporter) i
                writeHtmlBreak
                unless (null $ experimentDescription e) $
                  writeHtmlParagraph $
                  writeHtmlText $ experimentDescription e
                forM_ (zip [1..] reporters) $ \(i, reporter) ->
                  reporterWriteHtml (runWebPageContext $
                                     reporterContext reporter) i
           file = combine path "index.html"
       ((), contents) <- runHtmlWriter html id
       liftIO $ do
         UTF8.writeFile file (contents [])
         when (experimentVerbose e) $
           do putStr "Generated file "
              putStrLn file