
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Types
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- It defines a renderer that saves the results in files when running the simulation experiment.
--

module Simulation.Aivika.Experiment.FileRenderer where

import Simulation.Aivika.Experiment.Types

-- | It defines a simulation 'Experiment' renderer that saves the results in files. 
data FileRenderer a = FileRenderer a
                      -- ^ A file renderer that depends on the provided parameter.

-- | Saving the results of simulation in files when running the experiment.
instance ExperimentRendering (FileRenderer a) where

  -- | A file rendering context.
  data ExperimentContext (FileRenderer a) = FileContext
                                            -- ^ A file context constructor.
  
  renderExperiment e r reporters path = return ()
