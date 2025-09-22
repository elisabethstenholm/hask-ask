module Main where

import Auction
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock
import Server

main :: IO ()
main = do
  currentTime <- getCurrentTime
  let end = addUTCTime (secondsToNominalDiffTime 60) currentTime
  bid <- newEmptyTMVarIO
  st <- newTVarIO Open
  let itemTVar = Item {endTime = end, highestBid = bid, state = st}
  bidQueue <- newTChanIO

  _ <- forkIO $ handleBids itemTVar bidQueue
  runApp itemTVar bidQueue
