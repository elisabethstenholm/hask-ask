module Auction where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock
import Utilities

type Name = String

type Amount = Int

type Bid = (Name, Amount)

data ItemState = Open | Closed

data Item = Item
  { endTime :: UTCTime,
    highestBid :: TVar Bid,
    state :: TVar ItemState
  }

type BidQueue = TChan Bid

updateCurrentBid :: Item -> BidQueue -> STM ()
updateCurrentBid item queueVar = do
  itemState <- readTVar $ state item
  case itemState of
    Closed -> return ()
    Open -> do
      (name, amount) <- readTVar $ highestBid item
      (bidName, bidAmount) <- readTChan queueVar
      let newHighestBid = if amount >= bidAmount then (name, amount) else (bidName, bidAmount)
      writeTVar (highestBid item) newHighestBid

updateItem :: Item -> BidQueue -> IO Bid
updateItem item queueVar = do
  itemState <- readTVarIO $ state item
  case itemState of
    Closed -> readTVarIO $ highestBid item
    Open -> do
      atomically $ updateCurrentBid item queueVar
      updateItem item queueVar

getBids :: Item -> BidQueue -> IO ()
getBids item queueVar = do
  itemState <- readTVarIO $ state item
  case itemState of
    Closed -> return ()
    Open -> do
      (name, amount) <- readTVarIO (highestBid item)
      putStrLn $ "Current bid: " ++ show amount ++ " by " ++ name
      bidName <- getLine
      bidAmount <- getLine
      writeTChanIO queueVar (bidName, read bidAmount)
      threadDelay 10000
      getBids item queueVar

timer :: Item -> IO ()
timer item = do
  currentTime <- getCurrentTime
  if currentTime >= endTime item
    then
      writeTVarIO (state item) Closed
    else do
      threadDelay 100000
      timer item

tui :: IO ()
tui = do
  putStrLn "Today's auction item is a coffee cup made entirely of cat purrs!\n\nYou have 10 seconds!"

  currentTime <- getCurrentTime
  let itemEndTime = addUTCTime (secondsToNominalDiffTime 10) currentTime
  currentBidVar <- newTVarIO ("", 0)
  currentState <- newTVarIO Open
  queueVar <- newTChanIO

  let item = Item {endTime = itemEndTime, highestBid = currentBidVar, state = currentState}

  void $ forkIO $ forever $ timer item
  void $ forkIO $ do
    (winner, _) <- updateItem item queueVar
    putStrLn $ "Winning bid: " ++ winner
  getBids item queueVar