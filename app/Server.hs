module Server (runApp) where

import Auction
import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Utilities
import View

type API =
  Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> Get '[HTML] (Html ())
    :<|> "item" :> Capture "id" Integer :> "placeBid" :> ReqBody '[JSON] Bid :> PostNoContent
    :<|> "static" :> Raw

getHome :: Items -> Handler (Html ())
getHome items = do
  itms <- liftIO $ readTVarIO items
  let (itemIds, itemTVars) = unzip $ Map.toList itms
  itemsPure <- liftIO $ mapM itemTVarToPure itemTVars
  return $ withHead $ itemTableWithLink $ zip itemIds itemsPure

getItem :: Items -> Integer -> Handler (Html ())
getItem items itemId = do
  itms <- liftIO $ readTVarIO items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> do
      itemPure <- liftIO $ itemTVarToPure item
      return $ withHead $ do
        itemTableWithoutLink [itemPure]
        bidForm

postBid :: Items -> BidQueue -> Integer -> Bid -> Handler NoContent
postBid items queue itemId bid = do
  itms <- liftIO $ readTVarIO items
  case Map.lookup itemId itms of
    Nothing -> throwError err404
    Just item -> do
      liftIO $ writeTChanIO queue (item, bid)
      return NoContent

serveStatic :: Tagged Handler Application
serveStatic = serveDirectoryFileServer "static"

server :: Items -> BidQueue -> Server API
server items queue = getHome items :<|> getItem items :<|> postBid items queue :<|> serveStatic

api :: Proxy API
api = Proxy

app :: Items -> BidQueue -> Application
app = (serve api .) . server

runApp :: Items -> BidQueue -> IO ()
runApp = (run 8080 .) . app