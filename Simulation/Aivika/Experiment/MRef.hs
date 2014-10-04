
-- |
-- Module     : Simulation.Aivika.Experiment.MRef
-- Copyright  : Copyright (c) 2012-2014, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.8.3
--
-- The module defines a shared mutable reference accessible from different threads.
--

module Simulation.Aivika.Experiment.MRef
       (MRef,
        newMRef,
        readMRef,
        maybeReadMRef,
        writeMRef,
        maybeWriteMRef,
        modifyMRef) where

import Control.Concurrent.MVar

import Data.Maybe
import Data.IORef

-- | This is a shared mutable reference accessible from different threads.
data MRef a =
  MRef { mrefLock :: MVar (),
         -- ^ A reference lock.
         mrefData :: IORef a
         -- ^ The reference itself.
       }

-- | Create a new shared reference with the specified initial value.
newMRef :: a -> IO (MRef a)
newMRef a =
  do v <- newMVar ()
     r <- newIORef a
     return MRef { mrefLock = v,
                   mrefData = r }

-- | Read the contents of the shared reference.
readMRef :: MRef a -> IO a
readMRef = readIORef . mrefData

-- | Like 'maybe' but for the shared reference under assumption
-- that the reference is updated only once by its initial value.
maybeReadMRef :: b -> (a -> b) -> MRef (Maybe a) -> IO b
maybeReadMRef b0 f x =
  do a <- readIORef (mrefData x)
     case a of
       Just a -> return (f a)
       Nothing ->
         withMVar (mrefLock x) $ \() ->
         do a <- readIORef (mrefData x)
            case a of
              Just a -> return (f a)
              Nothing -> return b0

-- | Update the contents of the shared reference.
writeMRef :: MRef a -> a -> IO ()
writeMRef x a =
  withMVar (mrefLock x) $ \() ->
  writeIORef (mrefData x) a

-- | Update the contents if the reference was empty and then return a result of
-- applying the specified function to either the initial or current value.
maybeWriteMRef :: MRef (Maybe a) -> IO a -> (a -> b) -> IO b
maybeWriteMRef x m0 f =
  do a <- readIORef (mrefData x)
     case a of
       Just a -> return (f a)
       Nothing ->
         withMVar (mrefLock x) $ \() ->
         do a <- readIORef (mrefData x)
            case a of
              Just a -> return (f a)
              Nothing ->
                do a0 <- m0
                   writeIORef (mrefData x) (Just a0)
                   return (f a0)

-- | Modify the contents of the shared reference.
modifyMRef :: MRef a -> (a -> a) -> IO ()
modifyMRef x f =
  withMVar (mrefLock x) $ \() ->
  modifyIORef (mrefData x) f
