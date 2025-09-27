module Server (runApp) where

import Auction
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import View

newtype ItemReq = ItemReq {descriptionReq :: Text}

instance FromJSON ItemReq where
  parseJSON = withObject "ItemReq" $ \obj ->
    ItemReq <$> obj .: "description"

type API =
  Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> "placeBid" :> ReqBody '[JSON] Bid :> PostNoContent
    :<|> "postItem" :> ReqBody '[JSON] ItemReq :> PostNoContent
    :<|> "static" :> Raw

getHome :: Items -> Handler (Html ())
getHome items = do
  itms <- liftIO $ readTVarIO items
  let (itemIds, itemTVars) = unzip $ Map.toList itms
  itemsPure <- liftIO $ mapM (atomically . itemTVarToPure) itemTVars
  return $ withHead "/static/home.js" $ do
    itemTableWithLink $ zip itemIds itemsPure
    itemForm

getItem :: Items -> Integer -> Handler (Html ())
getItem items itemId = do
  itms <- liftIO $ readTVarIO items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> do
      itemPure <- liftIO $ atomically $ itemTVarToPure item
      return $ withHead "/static/item.js" $ do
        a_ [href_ "/"] "Home"
        itemTableWithoutLink [itemPure]
        bidForm

postBid :: Items -> BidQueue -> Integer -> Bid -> Handler NoContent
postBid items queue itemId bid = do
  Handler $ mapExceptT atomically $ tryAddToQueue items queue itemId bid
  return NoContent

postItem :: HighestItemId -> Items -> ItemReq -> Handler NoContent
postItem highestItemId items itemReq = do
  currentTime <- liftIO getCurrentTime
  let endsAt = addUTCTime (secondsToNominalDiffTime 300) currentTime
  liftIO $ atomically $ addItem endsAt highestItemId items (descriptionReq itemReq)
  return NoContent

serveStatic :: Tagged Handler Application
serveStatic = serveDirectoryFileServer "static"

server :: HighestItemId -> Items -> BidQueue -> Server API
server highestItemId items queue =
  getHome items
    :<|> getItem items
    :<|> postBid items queue
    :<|> postItem highestItemId items
    :<|> serveStatic

api :: Proxy API
api = Proxy

app :: HighestItemId -> Items -> BidQueue -> Application
app = ((serve api .) .) . server

runApp :: HighestItemId -> Items -> BidQueue -> IO ()
runApp = ((run 8080 .) .) . app