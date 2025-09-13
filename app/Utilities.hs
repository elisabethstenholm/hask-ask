module Utilities where

import Control.Concurrent.STM

writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO = (atomically .) . writeTChan