module Auction
  ( Bid (..),
    ItemState (..),
    BidQueue,
    Item (..),
    ItemPure,
    ItemTVar,
    Items,
    HighestItemId,
    itemTVarToPure,
    handleBids,
    tryAddToQueue,
    addItem
  )
where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Servant.Server
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

type HighestItemId = TVar Integer

type BidQueue = TChan (ItemTVar, Bid)

type STMWithServerError a = ExceptT ServerError STM a

itemTVarToPure :: ItemTVar -> STM ItemPure
itemTVarToPure item = do
  bid <- tryReadTMVar $ highestBid item
  st <- readTVar $ state item
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

handleBid :: BidQueue -> STM ()
handleBid queueVar = do
  (item, bid) <- readTChan queueVar
  itemState <- readTVar $ state item
  when (itemState == Open) $ do
    placeBidOnItem item bid

handleBids :: BidQueue -> IO ()
handleBids = forever . atomically . handleBid

tryAddToQueue :: Items -> BidQueue -> Integer -> Bid -> STMWithServerError ()
tryAddToQueue items queue itemId bid = do
  itms <- lift $ readTVar items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> lift $ writeTChan queue (item, bid)

addItem :: UTCTime -> HighestItemId -> Items -> Text -> STM ()
addItem endsAt highestItemId items desc = do
  highestId <- readTVar highestItemId
  itms <- readTVar items
  noBid <- newEmptyTMVar
  open <- newTVar Open
  let newHighestId = highestId + 1
  let item =
        Item
          { description = desc,
            endTime = endsAt,
            highestBid = noBid,
            state = open
          }
  let updatedMap = Map.insert newHighestId item itms
  writeTVar items updatedMap
  writeTVar highestItemId newHighestId
