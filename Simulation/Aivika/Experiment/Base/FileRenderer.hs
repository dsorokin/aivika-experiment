
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.Experiment.Base.FileRenderer
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It defines a renderer that saves the results in files when running the simulation experiment.
--

module Simulation.Aivika.Experiment.Base.FileRenderer where

import Control.Monad
import Control.Monad.Trans

import System.Directory
import System.FilePath

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Experiment.Types
import Simulation.Aivika.Experiment.Base.ExperimentWriter

-- | It defines a simulation 'Experiment' renderer that saves the results in files. 
data FileRenderer a = FileRenderer a ExperimentFilePath
                      -- ^ A file renderer that depends on the provided parameter and
                      -- a directory path, where the simulation results are saved in.

-- | A convenient type synonym for describing a file generator.
type FileGenerator a = ExperimentGenerator (FileRenderer a)

-- | Saving the results of simulation in files when running the experiment.
instance ExperimentRendering (FileRenderer a) where

  -- | A file rendering context.
  data ExperimentContext (FileRenderer a) = FileContext
                                            -- ^ A file context constructor.

  -- | A file environment.
  type ExperimentEnvironment (FileRenderer a) = FilePath

  -- | A file rendering monad.
  type ExperimentMonad (FileRenderer a) = ExperimentWriter

  liftExperiment r = runExperimentWriter

  prepareExperiment e (FileRenderer _ path0) =
    do path <- resolveFilePath "" path0
       liftIO $ do
         when (experimentVerbose e) $
           do putStr "Updating directory " 
              putStrLn path
         createDirectoryIfMissing True path
       return path
  
  renderExperiment e r reporters path = return ()

  onExperimentCompleted e r path = return ()

  onExperimentFailed e r path e' = throwComp e'

