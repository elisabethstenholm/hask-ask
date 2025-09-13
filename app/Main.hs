module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Utilities

type Name = String

type Amount = Float

type Bid = (Name, Amount)

type HighestBid = TVar Bid

type BidQueue = TChan Bid

updateCurrentBid :: HighestBid -> BidQueue -> IO ()
updateCurrentBid currentBidVar queueVar = atomically $ do
  (name, amount) <- readTVar currentBidVar
  (bidName, bidAmount) <- readTChan queueVar
  let newHighestBid = if amount >= bidAmount then (name, amount) else (bidName, bidAmount)
  writeTVar currentBidVar newHighestBid

getBids :: HighestBid -> BidQueue -> IO ()
getBids currentBidVar queueVar = do
  (name, amount) <- readTVarIO currentBidVar
  putStrLn $ "Current bid: " ++ show amount ++ " by " ++ name
  bidName <- getLine
  bidAmount <- getLine
  writeTChanIO queueVar (bidName, read bidAmount)
  threadDelay 10000
  getBids currentBidVar queueVar

main :: IO ()
main = do
  putStrLn "Today's auction item is a coffee cup made entirely of cat purrs!"

  currentBidVar <- newTVarIO ("", 0)
  queueVar <- newTChanIO

  forkIO $ forever $ updateCurrentBid currentBidVar queueVar
  getBids currentBidVar queueVar
