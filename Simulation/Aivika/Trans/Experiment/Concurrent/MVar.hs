
-- |
-- Module     : Simulation.Aivika.Trans.Experiment.Concurrent.MVar
-- Copyright  : Copyright (c) 2012-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines helper functions for working with synchronized variable 'MVar'.
--

module Simulation.Aivika.Trans.Experiment.Concurrent.MVar
       (maybeReadMVarComp,
        maybeReadMVarParameter,
        maybeReadMVarSimulation,
        maybeReadMVarDynamics,
        maybeReadMVarEvent,
        maybeReadMVarProcess,
        maybePutMVarComp,
        maybePutMVarParameter,
        maybePutMVarSimulation,
        maybePutMVarDynamics,
        maybePutMVarEvent,
        maybePutMVarProcess) where

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Simulation.Aivika.Trans

-- | Like 'maybe' but for the synchronized variable.
maybeReadMVarComp :: (MonadComp m, MonadIO m) => b -> (a -> m b) -> MVar (Maybe a) -> m b
{-# INLINABLE maybeReadMVarComp #-}
maybeReadMVarComp b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Like 'maybeReadMVarComp' but within the 'Parameter' computation.
maybeReadMVarParameter :: (MonadComp m, MonadIO m) => b -> (a -> Parameter m b) -> MVar (Maybe a) -> Parameter m b
{-# INLINABLE maybeReadMVarParameter #-}
maybeReadMVarParameter b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Like 'maybeReadMVarComp' but within the 'Simulation' computation.
maybeReadMVarSimulation :: (MonadComp m, MonadIO m) => b -> (a -> Simulation m b) -> MVar (Maybe a) -> Simulation m b
{-# INLINABLE maybeReadMVarSimulation #-}
maybeReadMVarSimulation b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Like 'maybeReadMVarComp' but within the 'Dynamics' computation.
maybeReadMVarDynamics :: (MonadComp m, MonadIO m) => b -> (a -> Dynamics m b) -> MVar (Maybe a) -> Dynamics m b
{-# INLINABLE maybeReadMVarDynamics #-}
maybeReadMVarDynamics b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Like 'maybeReadMVarComp' but within the 'Event' computation.
maybeReadMVarEvent :: (MonadComp m, MonadIO m) => b -> (a -> Event m b) -> MVar (Maybe a) -> Event m b
{-# INLINABLE maybeReadMVarEvent #-}
maybeReadMVarEvent b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Like 'maybeReadMVarComp' but within the 'Process' computation.
maybeReadMVarProcess :: (MonadDES m, MonadIO m) => b -> (a -> Process m b) -> MVar (Maybe a) -> Process m b
{-# INLINABLE maybeReadMVarProcess #-}
maybeReadMVarProcess b0 f x =
  do a <- liftIO $ readMVar x
     case a of
       Just a  -> f a
       Nothing -> return b0

-- | Update the contents if the variable was empty and then return a result of
-- applying the specified function to either the initial or current value.
maybePutMVarComp :: (MonadComp m, MonadIO m) => MVar (Maybe a) -> m a -> (a -> m b) -> m b
{-# INLINABLE maybePutMVarComp #-}
maybePutMVarComp x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadComp m, MonadIO m) => SomeException -> m a
                handle e = do liftIO $ putMVar x Nothing
                              throwComp e
            a0 <- catchComp m0 handle
            liftIO $ putMVar x (Just a0)
            f a0

-- | Like 'maybePutMVarComp' but within the 'Parameter' computation.
maybePutMVarParameter :: (MonadComp m, MonadIO m) => MVar (Maybe a) -> Parameter m a -> (a -> Parameter m b) -> Parameter m b
{-# INLINABLE maybePutMVarParameter #-}
maybePutMVarParameter x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadComp m, MonadIO m) => SomeException -> Parameter m a
                handle e = do liftIO $ putMVar x Nothing
                              throwParameter e
            a0 <- catchParameter m0 handle
            liftIO $ putMVar x (Just a0)
            f a0

-- | Like 'maybePutMVarComp' but within the 'Simulation' computation.
maybePutMVarSimulation :: (MonadComp m, MonadIO m) => MVar (Maybe a) -> Simulation m a -> (a -> Simulation m b) -> Simulation m b
{-# INLINABLE maybePutMVarSimulation #-}
maybePutMVarSimulation x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadComp m, MonadIO m) => SomeException -> Simulation m a
                handle e = do liftIO $ putMVar x Nothing
                              throwSimulation e
            a0 <- catchSimulation m0 handle
            liftIO $ putMVar x (Just a0)
            f a0

-- | Like 'maybePutMVarComp' but within the 'Dynamics' computation.
maybePutMVarDynamics :: (MonadComp m, MonadIO m) => MVar (Maybe a) -> Dynamics m a -> (a -> Dynamics m b) -> Dynamics m b
{-# INLINABLE maybePutMVarDynamics #-}
maybePutMVarDynamics x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadComp m, MonadIO m) => SomeException -> Dynamics m a
                handle e = do liftIO $ putMVar x Nothing
                              throwDynamics e
            a0 <- catchDynamics m0 handle
            liftIO $ putMVar x (Just a0)
            f a0

-- | Like 'maybePutMVarComp' but within the 'Event' computation.
maybePutMVarEvent :: (MonadComp m, MonadIO m) => MVar (Maybe a) -> Event m a -> (a -> Event m b) -> Event m b
{-# INLINABLE maybePutMVarEvent #-}
maybePutMVarEvent x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadComp m, MonadIO m) => SomeException -> Event m a
                handle e = do liftIO $ putMVar x Nothing
                              throwEvent e
            a0 <- catchEvent m0 handle
            liftIO $ putMVar x (Just a0)
            f a0

-- | Like 'maybePutMVarComp' but within the 'Process' computation.
maybePutMVarProcess :: (MonadDES m, MonadIO m) => MVar (Maybe a) -> Process m a -> (a -> Process m b) -> Process m b
{-# INLINABLE maybePutMVarProcess #-}
maybePutMVarProcess x m0 f =
  -- N.B. it should be actually masked to be protected from the asynchronous exceptions
  do a <- liftIO $ takeMVar x
     case a of
       Just a ->
         do liftIO $ putMVar x (Just a)
            f a
       Nothing ->
         do let handle :: (MonadDES m, MonadIO m) => SomeException -> Process m a
                handle e = do liftIO $ putMVar x Nothing
                              throwProcess e
            a0 <- catchProcess m0 handle
            liftIO $ putMVar x (Just a0)
            f a0
