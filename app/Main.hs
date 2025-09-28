module Main where

import Auction
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock
import Server
import qualified Data.Map as Map

main :: IO ()
main = do
  let desc = "Disco teapot"
  currentTime <- getCurrentTime
  let end = addUTCTime (secondsToNominalDiffTime 60) currentTime
  bid <- newEmptyTMVarIO
  st <- newTVarIO Open
  let itemTVar = Item {description = desc, endTime = end, highestBid = bid, state = st}
  bidQueue <- newTChanIO

  items <- newTVarIO $ Map.singleton 1 itemTVar
  highestItemId <- newTVarIO 1

  itemSubscriptions <- newTVarIO Map.empty
  itemListSubscriptions <- newTVarIO Map.empty

  _ <- forkIO $ handleBids bidQueue
  runApp itemListSubscriptions itemSubscriptions highestItemId items bidQueue

