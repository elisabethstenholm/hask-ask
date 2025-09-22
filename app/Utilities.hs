module Utilities where

import Control.Concurrent.STM

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO = (atomically .) . writeTVar

writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO = (atomically .) . writeTChan

tryReadTMVarIO :: TMVar a -> IO (Maybe a)
tryReadTMVarIO = atomically . tryReadTMVar

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y = if f y >= f x then y else x