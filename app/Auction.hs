{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auction (Bid (..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import GHC.Generics
import Utilities
import Web.FormUrlEncoded

data Bid = Bid
  { name :: Text,
    amount :: Integer
  }
  deriving (Generic)

instance ToJSON Bid

instance FromForm Bid

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
      topBid <- readTVar $ highestBid item
      bid <- readTChan queueVar
      let newHighestBid = if amount topBid >= amount bid then topBid else bid
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
      topBid <- readTVarIO (highestBid item)
      putStrLn $ "Current bid: " ++ show (amount topBid) ++ " by " ++ Text.unpack (name topBid)
      bidName <- getLine
      bidAmount <- getLine
      writeTChanIO queueVar $ Bid (Text.pack bidName) (read bidAmount)
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
  currentBidVar <- newTVarIO $ Bid "" 0
  currentState <- newTVarIO Open
  queueVar <- newTChanIO

  let item = Item {endTime = itemEndTime, highestBid = currentBidVar, state = currentState}

  void $ forkIO $ forever $ timer item
  void $ forkIO $ do
    winningBid <- updateItem item queueVar
    putStrLn $ "Winning bid: " ++ Text.unpack (name winningBid)
  getBids item queueVar