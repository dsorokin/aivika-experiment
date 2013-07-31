
-- |
-- Module     : Simulation.Aivika.Experiment.ListSource
-- Copyright  : Copyright (c) 2013, David Sorokin <david.sorokin@gmail.com>
-- License    : GPL
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 7.6.3
--
-- It represents an optimized source of data to construct the list.
--

module Simulation.Aivika.Experiment.ListSource
       (ListSource,
        providerToDoubleListSource,
        providerToIntListSource,
        ListData,
        listSourceData,
        listDataList,
        ListRef,
        newListRef,
        addDataToListRef,
        readListRef) where

import Data.IORef
import Control.Monad

import Simulation.Aivika.Event
import Simulation.Aivika.Statistics
import Simulation.Aivika.Experiment

-- | Represents the optimized source of data for constructing the list.
data ListSource a = SingleValueSource (Event a)
                  | MultipleValueSource (Event [a])

-- | Represents the optimized data by which the list will be created.
data ListData a = SingleValueData !a
                | MultipleValueData [a]

-- | Try to return the source of list data by the specified provider.
providerToDoubleListSource :: SeriesProvider -> Maybe (ListSource Double)
providerToDoubleListSource provider =
  case providerToDouble provider of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case providerToDoubleList provider of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing
        
-- | Try to return the source of list data by the specified provider.
providerToIntListSource :: SeriesProvider -> Maybe (ListSource Int)
providerToIntListSource provider =
  case providerToInt provider of
    Just x -> Just $ SingleValueSource x
    Nothing ->
      case providerToIntList provider of
        Just x -> Just $ MultipleValueSource x
        Nothing -> Nothing

-- | Get data from the source in the current time point.
listSourceData :: ListSource a -> Event (ListData a)
listSourceData (SingleValueSource x)    = x >>= return . SingleValueData
listSourceData (MultipleValueSource xs) =
  do ys <- xs
     zs <- forM ys $ \y -> return $ y `seq` y
     return $ MultipleValueData ys  -- it contains strict data, which is important for big models 

-- | Return the list of values contained in the specified data.
listDataList :: ListData a -> [a]
listDataList (SingleValueData x)    = [x]
listDataList (MultipleValueData xs) = xs

-- | Represents a reference to the list, optimized to work with the source of data.
newtype ListRef a = ListRef { listRef :: IORef [ListData a] } 

-- | Create a new list reference.
newListRef :: IO (ListRef a)
newListRef = fmap ListRef $ newIORef []

-- | Add data to the list reference.
addDataToListRef :: ListRef a -> ListData a -> IO ()
addDataToListRef (ListRef r) a = modifyIORef r (a :)

-- | Read the list contained in the reference.
readListRef :: ListRef a -> IO [a]
readListRef (ListRef r) =
  do x <- readIORef r
     return $ concat $ map listDataList x 
