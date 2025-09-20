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

data BidReq = BidReq
  { name :: Text,
    amount :: Text
  }
  deriving (Generic)

instance ToJSON BidReq

bids :: [Bid]
bids =
  [ Bid "Elli" 500,
    Bid "Håkon" 600
  ]

type API =
  Get '[HTML] (Html ())
    :<|> "allBids" :> Get '[JSON] [Bid]
    :<|> "placeBid" :> ReqBody '[FormUrlEncoded] Bid :> Post '[HTML] (Html ())

getHome :: Handler (Html ())
getHome = return $ withHead bidForm

getBids :: Handler [Bid]
getBids = return bids

postBid :: Bid -> Handler (Html ())
postBid _ =
  return $
    doctypehtml_ $ do
      head_ (title_ "Hask Ask")
      body_ (p_ "Yay! You placed a bid")

server :: Server API
server = getHome :<|> getBids :<|> postBid

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

runApp :: IO ()
runApp = run 8080 app