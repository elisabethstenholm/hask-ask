module Server (runApp) where

import Auction
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import View

data ItemReq = ItemReq
  { descriptionReq :: Text,
    askingPriceReq :: Integer
  }

instance FromJSON ItemReq where
  parseJSON = withObject "ItemReq" $ \obj ->
    ItemReq <$> obj .: "description" <*> obj .: "askingPrice"

newtype SubscriptionId = SubscriptionId {subscriptionId :: UUID}

instance FromJSON SubscriptionId where
  parseJSON = withObject "SubscriptionId" $ \obj -> do
    txt <- obj .: "subscriptionId"
    case UUID.fromText txt of
      Just uuid -> return $ SubscriptionId uuid
      Nothing -> fail "Invalid UUID in field 'subscriptionId'"

type API =
  Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> "placeBid" :> ReqBody '[JSON] Bid :> PostNoContent
    :<|> "itemSubscription" :> "poll" :> ReqBody '[JSON] SubscriptionId :> Post '[JSON] ItemPure
    :<|> "itemListSubscription" :> "poll" :> ReqBody '[JSON] SubscriptionId :> Post '[HTML] (Html ())
    :<|> "postItem" :> ReqBody '[JSON] ItemReq :> PostNoContent
    :<|> "static" :> Raw

getHome :: ItemListSubscriptions -> ItemSubscriptions -> HighestItemId -> Items -> Handler (Html ())
getHome itemListSubscr itemSubscr hghId items = do
  (itemListSubscrId, itms) <- liftIO $ subscribeToItemList itemListSubscr hghId items
  let (itemIds, itemTVars) = unzip $ Map.toList itms
  itemsAndSubscrIds <- liftIO $ mapM (subscribeToItem itemSubscr) itemTVars
  return $ withHead "/static/home.js" $ do
    itemTableWithLink itemListSubscrId $ zipWith (\x (y, z) -> (x, y, z)) itemIds itemsAndSubscrIds
    itemForm

getItem :: ItemSubscriptions -> Items -> Integer -> Handler (Html ())
getItem subscriptions items itemId = do
  itms <- liftIO $ readTVarIO items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> do
      (uuid, itemPure) <- liftIO $ subscribeToItem subscriptions item
      return $ withHead "/static/item.js" $ do
        a_ [href_ "/"] "Home"
        itemTableWithoutLink [(itemPure, uuid)]
        bidForm

postBid :: Items -> BidQueue -> Integer -> Bid -> Handler NoContent
postBid items queue itemId bid = do
  Handler $ mapExceptT atomically $ tryAddToQueue items queue itemId bid
  return NoContent

pollItem :: ItemSubscriptions -> SubscriptionId -> Handler ItemPure
pollItem subscriptions subscrId = do
  Handler $
    mapExceptT atomically $
      pollItemSubscription subscriptions (subscriptionId subscrId)

pollItems :: ItemListSubscriptions -> ItemSubscriptions -> Items -> SubscriptionId -> Handler (Html ())
pollItems itemListSubscr itemSubscr items subscrId = do
  (itemId, item) <-
    Handler $
      mapExceptT atomically $
        pollItemListSubscription itemListSubscr items (subscriptionId subscrId)
  (itemSubscrId, itemPure) <- liftIO $ subscribeToItem itemSubscr item
  return $ itemRow (Just $ itemLink itemId) itemPure itemSubscrId

postItem :: HighestItemId -> Items -> ItemReq -> Handler NoContent
postItem highestItemId items itemReq = do
  currentTime <- liftIO getCurrentTime
  let endsAt = addUTCTime (secondsToNominalDiffTime 300) currentTime
  let desc = descriptionReq itemReq
  let askPr = askingPriceReq itemReq
  liftIO $ atomically $ addItem endsAt highestItemId items desc askPr
  return NoContent

serveStatic :: Tagged Handler Application
serveStatic = serveDirectoryFileServer "static"

server :: ItemListSubscriptions -> ItemSubscriptions -> HighestItemId -> Items -> BidQueue -> Server API
server itemListSubscr itemSubscr highestItemId items queue =
  getHome itemListSubscr itemSubscr highestItemId items
    :<|> getItem itemSubscr items
    :<|> postBid items queue
    :<|> pollItem itemSubscr
    :<|> pollItems itemListSubscr itemSubscr items
    :<|> postItem highestItemId items
    :<|> serveStatic

api :: Proxy API
api = Proxy

app :: ItemListSubscriptions -> ItemSubscriptions -> HighestItemId -> Items -> BidQueue -> Application
app = ((((serve api .) .) .) .) . server

runApp :: ItemListSubscriptions -> ItemSubscriptions -> HighestItemId -> Items -> BidQueue -> IO ()
runApp = ((((run 8080 .) .) .) .) . app