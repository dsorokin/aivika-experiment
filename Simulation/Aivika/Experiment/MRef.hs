
-- |
-- Module     : Simulation.Aivika.Experiment.MRef
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
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
        modifyMRef_,
        modifyMRef,
        withMRef) where

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
maybeReadMRef :: b -> (a -> IO b) -> MRef (Maybe a) -> IO b
maybeReadMRef b0 f x =
  do a <- readIORef (mrefData x)
     case a of
       Just a -> f a
       Nothing ->
         withMVar (mrefLock x) $ \() ->
         do a <- readIORef (mrefData x)
            case a of
              Just a -> f a
              Nothing -> return b0

-- | Update the contents of the shared reference.
writeMRef :: MRef a -> a -> IO ()
writeMRef x a =
  withMVar (mrefLock x) $ \() ->
  writeIORef (mrefData x) a

-- | Update the contents if the reference was empty and then return a result of
-- applying the specified function to either the initial or current value.
maybeWriteMRef :: MRef (Maybe a) -> IO a -> (a -> IO b) -> IO b
maybeWriteMRef x m0 f =
  do a <- readIORef (mrefData x)
     case a of
       Just a -> f a
       Nothing ->
         withMVar (mrefLock x) $ \() ->
         do a <- readIORef (mrefData x)
            case a of
              Just a -> f a
              Nothing ->
                do a0 <- m0
                   writeIORef (mrefData x) (Just a0)
                   f a0

-- | Modify the contents of the shared reference.
modifyMRef_ :: MRef a -> (a -> IO a) -> IO ()
modifyMRef_ x f =
  withMVar (mrefLock x) $ \() ->
  do a  <- readIORef (mrefData x)
     a' <- f a
     writeIORef (mrefData x) a'

-- | Modify the contents of the shared reference but allow returning the result.
modifyMRef :: MRef a -> (a -> IO (a, b)) -> IO b
modifyMRef x f =
  withMVar (mrefLock x) $ \() ->
  do a <- readIORef (mrefData x)
     (a', b) <- f a
     writeIORef (mrefData x) a'
     return b

-- | A safe wrapper for operating with the contents of shared reference.
withMRef :: MRef a -> (a -> IO b) -> IO b
withMRef x f =
  withMVar (mrefLock x) $ \() ->
  readIORef (mrefData x) >>= f
