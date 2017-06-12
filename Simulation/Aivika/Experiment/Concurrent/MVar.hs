
-- |
-- Module     : Simulation.Aivika.Experiment.Concurrent.MVar
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines helper functions for working with synchronized variable 'MVar'.
--

module Simulation.Aivika.Experiment.Concurrent.MVar
       (maybeReadMVar,
        maybePutMVar) where

import Control.Exception
import Control.Concurrent.MVar

import Data.Maybe

-- | Like 'maybe' but for the synchronized variable.
maybeReadMVar :: b -> (a -> IO b) -> MVar (Maybe a) -> IO b
maybeReadMVar b0 f x =
  do a <- readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Update the contents if the variable was empty and then return a result of
-- applying the specified function to either the initial or current value.
maybePutMVar :: MVar (Maybe a) -> IO a -> (a -> IO b) -> IO b
maybePutMVar x m0 f =
  mask_ $
  do a <- takeMVar x
     case a of
       Just a ->
         do putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: SomeException -> IO a
                handle e = do putMVar x Nothing
                              throw e
            a0 <- catch m0 handle
            putMVar x (Just a0)
            f a0
