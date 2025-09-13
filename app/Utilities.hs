module Utilities where

import Control.Concurrent.STM

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO = (atomically .) . writeTVar

writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO = (atomically .) . writeTChan