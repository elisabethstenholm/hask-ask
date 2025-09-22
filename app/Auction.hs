module Auction
  ( Bid (..),
    ItemState (..),
    BidQueue,
    Item (..),
    ItemId,
    ItemTVar,
    itemTVarToId,
    handleBids,
  )
where

-- import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Functor.Identity
import Data.Text (Text)
-- import qualified Data.Text as Text
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
  deriving (Generic, Eq)

instance ToJSON ItemState

data Item f g = Item
  { endTime :: UTCTime,
    highestBid :: f Bid,
    state :: g ItemState
  }

type ItemId = Item Maybe Identity

type ItemTVar = Item TMVar TVar

instance ToJSON ItemId where
  toJSON item =
    object
      [ "endTime" .= endTime item,
        "highestBid" .= highestBid item,
        "state" .= runIdentity (state item)
      ]

itemTVarToId :: ItemTVar -> IO ItemId
itemTVarToId item = do
  bid <- tryReadTMVarIO $ highestBid item
  st <- readTVarIO $ state item
  return $
    Item
      { endTime = endTime item,
        highestBid = bid,
        state = Identity st
      }

type BidQueue = TChan Bid

updateCurrentBid :: ItemTVar -> BidQueue -> STM ()
updateCurrentBid item queueVar = do
  itemState <- readTVar $ state item
  when (itemState == Open) $ do
    maybeBid <- tryReadTMVar $ highestBid item
    bid <- readTChan queueVar
    let newHighestBid = maybe bid (maxOn amount bid) maybeBid
    writeTMVar (highestBid item) newHighestBid

handleBids :: ItemTVar -> BidQueue -> IO ()
handleBids item queueVar = do
  itemState <- readTVarIO $ state item
  when (itemState == Open) $ do
    atomically $ updateCurrentBid item queueVar
    handleBids item queueVar

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