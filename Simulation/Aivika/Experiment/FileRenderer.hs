
-- |
-- Module     : Simulation.Aivika.Experiment.FileRenderer
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It defines a basic experiment renderer that allows saving the simulation results in files.
--

module Simulation.Aivika.Experiment.FileRenderer
       (FileRenderer(..),
        HtmlRenderer(..)) where

-- | The experiment renderer that allows saving the simulation results in files.
class FileRenderer a

-- | The default renderer that creates an HTML page for the simulation experiment.
data HtmlRenderer = HtmlRenderer
                    -- ^ The default HTML renderer.

instance FileRenderer HtmlRenderer
