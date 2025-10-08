module Utilities where

import Control.Concurrent.STM
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO = (atomically .) . writeTVar

maxOn :: (Ord b) => (a -> b) -> a -> a -> a
maxOn f x y = if f y >= f x then y else x

lookupOrThrow :: (Ord k, MonadError e m) => k -> Map k a -> e -> m a
lookupOrThrow key mp err = maybe (throwError err) pure (Map.lookup key mp)