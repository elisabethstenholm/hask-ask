module Main where

import Auction
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Time.Clock
import Server

main :: IO ()
main = do
  let desc = "Disco teapot"
  let askPr = 100
  currentTime <- getCurrentTime
  let end = addUTCTime (secondsToNominalDiffTime 10) currentTime
  bid <- newEmptyTMVarIO
  st <- newTVarIO Open
  let itemTVar =
        Item
          { description = desc,
            askingPrice = askPr,
            endTime = end,
            highestBid = bid,
            state = st
          }
  bidQueue <- newTChanIO

  items <- newTVarIO $ Map.singleton 1 itemTVar
  highestItemId <- newTVarIO 1
  _ <- forkIO $ closeWhenPastEndTime itemTVar

  itemSubscriptions <- newTVarIO Map.empty
  itemListSubscriptions <- newTVarIO Map.empty

  _ <- forkIO $ handleBids bidQueue
  runApp itemListSubscriptions itemSubscriptions highestItemId items bidQueue
