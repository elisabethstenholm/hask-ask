module Server (runApp) where

import Auction
import Control.Monad.IO.Class
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Utilities
import View

type API =
  Get '[HTML] (Html ())
    :<|> "item" :> Get '[JSON] ItemId
    :<|> "placeBid" :> ReqBody '[JSON] Bid :> PostNoContent
    :<|> "static" :> Raw

getHome :: Handler (Html ())
getHome = return $ withHead bidForm

getItem :: ItemTVar -> Handler ItemId
getItem = liftIO . itemTVarToId

postBid :: BidQueue -> Bid -> Handler NoContent
postBid queue bid = do
  liftIO $ writeTChanIO queue bid
  return NoContent

serveStatic :: Tagged Handler Application
serveStatic = serveDirectoryFileServer "static"

server :: ItemTVar -> BidQueue -> Server API
server item queue = getHome :<|> getItem item :<|> postBid queue :<|> serveStatic

api :: Proxy API
api = Proxy

app :: ItemTVar -> BidQueue -> Application
app = (serve api .) . server

runApp :: ItemTVar -> BidQueue -> IO ()
runApp = (run 8080 .) . app