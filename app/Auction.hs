module Auction
  ( Bid (..),
    ItemState (..),
    BidQueue,
    Item (..),
    ItemPure,
    ItemTVar,
    Items,
    HighestItemId,
    ItemSubscriptions,
    ItemListSubscriptions,
    itemTVarToPure,
    handleBids,
    tryAddToQueue,
    addItem,
    subscribeToItem,
    pollItemSubscription,
    subscribeToItemList,
    pollItemListSubscription,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDV4
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
    askingPrice :: Integer,
    endTime :: UTCTime,
    highestBid :: f Bid,
    state :: g ItemState
  }

type ItemPure = Item Maybe Identity

deriving instance Eq ItemPure

instance ToJSON ItemPure where
  toJSON item =
    object
      [ "description" .= description item,
        "askingPrice" .= askingPrice item,
        "endTime" .= formatTime defaultTimeLocale "%D %R" (endTime item),
        "highestBid" .= highestBid item,
        "state" .= runIdentity (state item)
      ]

type ItemTVar = Item TMVar TVar

type Items = TVar (Map Integer ItemTVar)

type HighestItemId = TVar Integer

type ItemSubscriptions = TVar (Map UUID (TMVar ItemPure))

type ItemListSubscriptions = TVar (Map UUID (TChan Integer))

type BidQueue = TChan (ItemTVar, Bid)

type STMServErr a = ExceptT ServerError STM a

itemTVarToPure :: ItemTVar -> STM ItemPure
itemTVarToPure item = do
  bid <- tryReadTMVar $ highestBid item
  st <- readTVar $ state item
  return $
    Item
      { description = description item,
        askingPrice = askingPrice item,
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

tryAddToQueue :: Items -> BidQueue -> Integer -> Bid -> STMServErr ()
tryAddToQueue items queue itemId bid = do
  itms <- lift $ readTVar items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> do
      when (amount bid < askingPrice item) $
        throwError $
          err409 {errBody = "Bid below asking price"}
      lift $ writeTChan queue (item, bid)

addItem :: UTCTime -> HighestItemId -> Items -> Text -> Integer -> STM ()
addItem endsAt highestItemId items desc askPr = do
  highestId <- readTVar highestItemId
  itms <- readTVar items
  noBid <- newEmptyTMVar
  open <- newTVar Open
  let newHighestId = highestId + 1
  let item =
        Item
          { description = desc,
            askingPrice = askPr,
            endTime = endsAt,
            highestBid = noBid,
            state = open
          }
  let updatedMap = Map.insert newHighestId item itms
  writeTVar items updatedMap
  writeTVar highestItemId newHighestId

updateItemSubscription :: ItemTVar -> TMVar ItemPure -> ItemPure -> IO ()
updateItemSubscription itemTVar itemBox lastItemVersion = do
  newItemPure <- atomically $ do
    currentItemVersion <- itemTVarToPure itemTVar
    guard $ currentItemVersion /= lastItemVersion
    writeTMVar itemBox currentItemVersion
    return currentItemVersion
  updateItemSubscription itemTVar itemBox newItemPure

subscribeToItem :: ItemSubscriptions -> ItemTVar -> IO (UUID, ItemPure)
subscribeToItem subscriptions itemTVar = do
  uuid <- UUIDV4.nextRandom
  itemBox <- newEmptyTMVarIO
  itemPure <- atomically $ itemTVarToPure itemTVar
  atomically $ modifyTVar subscriptions $ Map.insert uuid itemBox
  _ <- forkIO $ updateItemSubscription itemTVar itemBox itemPure
  return (uuid, itemPure)

pollItemSubscription :: ItemSubscriptions -> UUID -> STMServErr ItemPure
pollItemSubscription subscriptions subscriptionId = do
  subscr <- lift $ readTVar subscriptions
  case Map.lookup subscriptionId subscr of
    Nothing -> throwError err404
    Just itemBox -> lift $ takeTMVar itemBox

updateItemListSubscription :: TChan Integer -> HighestItemId -> Integer -> IO ()
updateItemListSubscription itemChan highestIdTVar lastHighestId = do
  newHighestId <- atomically $ do
    currentHighestId <- readTVar highestIdTVar
    guard $ currentHighestId > lastHighestId
    mapM_ (writeTChan itemChan) [lastHighestId + 1 .. currentHighestId]
    return currentHighestId
  updateItemListSubscription itemChan highestIdTVar newHighestId

subscribeToItemList :: ItemListSubscriptions -> HighestItemId -> Items -> IO (UUID, Map Integer ItemTVar)
subscribeToItemList subscriptions highestId items = do
  uuid <- UUIDV4.nextRandom
  itemChan <- newTChanIO
  atomically $ modifyTVar subscriptions $ Map.insert uuid itemChan
  (currentHighestId, itms) <- atomically $ do
    currentHighestId <- readTVar highestId
    itms <- readTVar items
    return (currentHighestId, itms)
  _ <- forkIO $ updateItemListSubscription itemChan highestId currentHighestId
  return (uuid, itms)

pollItemListSubscription :: ItemListSubscriptions -> Items -> UUID -> STMServErr (Integer, ItemTVar)
pollItemListSubscription subscriptions items subscriptionId = do
  subscr <- lift $ readTVar subscriptions
  itms <- lift $ readTVar items
  case Map.lookup subscriptionId subscr of
    Nothing -> throwError err404
    Just itemChan -> do
      itemId <- lift $ readTChan itemChan
      case Map.lookup itemId itms of
        Nothing -> throwError err500
        Just itemTVar -> return (itemId, itemTVar)
