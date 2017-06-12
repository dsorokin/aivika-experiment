
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Base.WebPageRenderer
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines a renderer that creates a web page when running the simulation experiment.
--

module Simulation.Aivika.Experiment.Base.WebPageRenderer where

import Control.Monad
import Control.Monad.Trans

import System.IO
import System.Directory
import System.FilePath

import Simulation.Aivika
import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.HtmlWriter
import Simulation.Aivika.Experiment.Base.ExperimentWriter

-- | It defines the web page renderer for simulation 'Experiment'. 
data WebPageRenderer a = WebPageRenderer a ExperimentFilePath
                         -- ^ A renderer that depends on the provided parameter and
                         -- a directory path, where the simulation results are saved in.

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

-- | Rendering a web page with results when running the simulation experiment.
instance ExperimentRendering (WebPageRenderer a) where

  -- | A web page context.
  newtype ExperimentContext (WebPageRenderer a) =
    WebPageContext { runWebPageContext :: WebPageWriter
                     -- ^ Run the web page context.
                   }

  -- | A web page environment.
  type ExperimentEnvironment (WebPageRenderer a) = FilePath

  -- | A web page rendering monad.
  type ExperimentMonad (WebPageRenderer a) = ExperimentWriter

  liftExperiment r = runExperimentWriter

  prepareExperiment e (WebPageRenderer _ path0) =
    do path <- resolveFilePath "" path0
       liftIO $ do
         when (experimentVerbose e) $
           do putStr "Updating directory " 
              putStrLn path
         createDirectoryIfMissing True path
       return path

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
       liftIO $
         withFile file WriteMode $ \h ->
         do hSetEncoding h utf8
            hPutStr h (contents [])
            when (experimentVerbose e) $
              do putStr "Generated file "
                 putStrLn file

  onExperimentCompleted e r path = return ()

  onExperimentFailed e r path e' = throwComp e'
