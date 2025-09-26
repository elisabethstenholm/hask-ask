module Auction
  ( Bid (..),
    ItemState (..),
    BidQueue,
    Item (..),
    ItemPure,
    ItemTVar,
    Items,
    itemTVarToPure,
    handleBids,
  )
where

import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Utilities
import Web.FormUrlEncoded

data Bid = Bid
  { name :: Text,
    amount :: Integer
  }
  deriving (Generic, Eq)

instance ToJSON Bid

instance FromJSON Bid

instance FromForm Bid

data ItemState = Open | Closed
  deriving (Generic, Eq, Show)

instance ToJSON ItemState

data Item f g = Item
  { description :: Text,
    endTime :: UTCTime,
    highestBid :: f Bid,
    state :: g ItemState
  }

type ItemPure = Item Maybe Identity

instance ToJSON ItemPure where
  toJSON item =
    object
      [ "description" .= description item,
        "endTime" .= endTime item,
        "highestBid" .= highestBid item,
        "state" .= runIdentity (state item)
      ]

type ItemTVar = Item TMVar TVar

type Items = TVar (Map Integer ItemTVar)

type BidQueue = TChan (ItemTVar, Bid)

itemTVarToPure :: ItemTVar -> IO ItemPure
itemTVarToPure item = do
  bid <- tryReadTMVarIO $ highestBid item
  st <- readTVarIO $ state item
  return $
    Item
      { description = description item,
        endTime = endTime item,
        highestBid = bid,
        state = Identity st
      }

placeBidOnItem :: ItemTVar -> Bid -> STM ()
placeBidOnItem item bid = do
  itemState <- readTVar $ state item
  when (itemState == Open) $ do
    maybeBid <- tryReadTMVar $ highestBid item
    let newHighestBid = maybe bid (maxOn amount bid) maybeBid
    writeTMVar (highestBid item) newHighestBid

handleBids :: BidQueue -> IO ()
handleBids queueVar = do
  (item, bid) <- readTChanIO queueVar
  itemState <- readTVarIO $ state item
  when (itemState == Open) $ do
    atomically $ placeBidOnItem item bid
    handleBids queueVar

-- getBids :: ItemTVar -> BidQueue -> IO ()
-- getBids item queueVar = do
--   itemState <- readTVarIO $ state item
--   case itemState of
--     Closed -> return ()
--     Open -> do
--       topBid <- readTVarIO (highestBid item)
--       putStrLn $ "Current bid: " ++ show (amount topBid) ++ " by " ++ Text.unpack (name topBid)
--       bidName <- getLine
--       bidAmount <- getLine
--       writeTChanIO queueVar $ Bid (Text.pack bidName) (read bidAmount)
--       threadDelay 10000
--       getBids item queueVar

-- timer :: ItemTVar -> IO ()
-- timer item = do
--   currentTime <- getCurrentTime
--   if currentTime >= endTime item
--     then
--       writeTVarIO (state item) Closed
--     else do
--       threadDelay 100000
--       timer item

-- tui :: IO ()
-- tui = do
--   putStrLn "Today's auction item is a coffee cup made entirely of cat purrs!\n\nYou have 10 seconds!"

--   currentTime <- getCurrentTime
--   let itemEndTime = addUTCTime (secondsToNominalDiffTime 10) currentTime
--   currentBidVar <- newTVarIO $ Bid "" 0
--   currentState <- newTVarIO Open
--   queueVar <- newTChanIO

--   let item = Item {endTime = itemEndTime, highestBid = currentBidVar, state = currentState}

--   void $ forkIO $ forever $ timer item
--   void $ forkIO $ do
--     winningBid <- updateItem item queueVar
--     putStrLn $ "Winning bid: " ++ Text.unpack (name winningBid)
--   getBids item queueVar