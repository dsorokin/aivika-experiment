
-- |
-- Module     : Simulation.Aivika.Experiment.ExperimentWriter
-- Copyright  : Copyright (c) 2012-2015, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.10.1
--
-- It defines the 'Exp' monad that allows providing computation with
-- an ability to resolve file paths.
--
module Simulation.Aivika.Experiment.ExperimentWriter
       (ExperimentWriter,
        runExperimentWriter,
        ExperimentFilePath(..),
        resolveFilePath,
        expandFilePath,
        mapFilePath) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import System.Directory
import System.FilePath

import Simulation.Aivika.Experiment.MRef
import Simulation.Aivika.Experiment.Utils (replace)
  
-- | Specifies the file name, unique or writable, which can be appended with extension if required.
data ExperimentFilePath = WritableFilePath FilePath
                          -- ^ The file which is overwritten in 
                          -- case if it existed before.
                        | UniqueFilePath FilePath
                          -- ^ The file which is always unique,
                          -- when an automatically generated suffix
                          -- is added to the name in case of need.
                
-- | Resolve the file path relative to the specified directory passed in the first argument
-- and taking into account a possible requirement to have an unique file name.
resolveFilePath :: FilePath -> ExperimentFilePath -> ExperimentWriter FilePath
resolveFilePath dir (WritableFilePath path) =
  return $ dir </> path
resolveFilePath dir (UniqueFilePath path)   =
  ExperimentWriter $ \r ->
  let (name, ext) = splitExtension path
      loop y i =
        do let n  = dir </> addExtension y ext
               y' = name ++ "(" ++ show i ++ ")"
           f1 <- doesFileExist n
           f2 <- doesDirectoryExist n
           if f1 || f2
             then loop y' (i + 1)
             else do n' <- liftIO $
                           modifyMRef r $ \s ->
                           if S.member n s
                           then return (s, Nothing)
                           else return (S.insert n s, Just n)
                     case n' of
                       Nothing -> loop y' (i + 1)
                       Just n' -> return n'
  in loop name 2

-- | Expand the file path using the specified table of substitutions.
expandFilePath :: ExperimentFilePath -> M.Map String String -> ExperimentFilePath
expandFilePath (WritableFilePath path) map = WritableFilePath (expandTemplates path map)
expandFilePath (UniqueFilePath path) map = UniqueFilePath (expandTemplates path map)

-- | Expand the string templates using the specified table of substitutions.
expandTemplates :: String -> M.Map String String -> String     
expandTemplates name map = name' where
  ((), name') = flip runState name $
                forM_ (M.assocs map) $ \(k, v) ->
                do a <- get
                   put $ replace k v a

-- | Transform the file path using the specified function.
mapFilePath :: (FilePath -> FilePath) -> ExperimentFilePath -> ExperimentFilePath
mapFilePath f (WritableFilePath path) = WritableFilePath (f path)
mapFilePath f (UniqueFilePath path) = UniqueFilePath (f path) 


-- | Defines an 'IO' derived computation whithin which we can resolve the unique file paths.
newtype ExperimentWriter a = ExperimentWriter (MRef (S.Set String) -> IO a)

instance Functor ExperimentWriter where

  {-# INLINE fmap #-}
  fmap f (ExperimentWriter m) =
    ExperimentWriter $ \r -> fmap f (m r)

instance Applicative ExperimentWriter where

  {-# INLINE pure #-}
  pure a =
    ExperimentWriter $ \r -> return a

  {-# INLINE (<*>) #-}
  (ExperimentWriter f) <*> (ExperimentWriter m) =
    ExperimentWriter $ \r -> f r <*> m r

instance Monad ExperimentWriter where

  {-# INLINE return #-}
  return a =
    ExperimentWriter $ \r -> return a

  {-# INLINE (>>=) #-}
  (ExperimentWriter m) >>= k =
    ExperimentWriter $ \r ->
    do a <- m r
       let ExperimentWriter b = k a
       b r
       
instance MonadIO ExperimentWriter where

  {-# INLINE liftIO #-}
  liftIO m = ExperimentWriter $ \r -> liftIO m

-- | Run the 'ExperimentWriter' computation.
runExperimentWriter :: ExperimentWriter a -> IO a
runExperimentWriter (ExperimentWriter m) =
  do r <- newMRef S.empty
     m r
