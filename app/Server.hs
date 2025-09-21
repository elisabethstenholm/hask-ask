module Server (runApp) where

import Auction
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import View


bids :: [Bid]
bids =
  [ Bid "Elli" 500,
    Bid "Håkon" 600
  ]

type API =
  Get '[HTML] (Html ())
    :<|> "allBids" :> Get '[JSON] [Bid]
    :<|> "placeBid" :> ReqBody '[FormUrlEncoded] Bid :> PostNoContent
    :<|> "static" :> Raw

getHome :: Handler (Html ())
getHome = return $ withHead bidForm

getBids :: Handler [Bid]
getBids = return bids

postBid :: Bid -> Handler NoContent
postBid _ = return NoContent

serveStatic :: Tagged Handler Application
serveStatic = serveDirectoryFileServer "static"

server :: Server API
server = getHome :<|> getBids :<|> postBid :<|> serveStatic

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app