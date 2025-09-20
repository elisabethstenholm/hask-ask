{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (runApp) where

import Auction
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

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
getHome =
  return $
    doctypehtml_ $ do
      head_ (title_ "Hask Ask")
      body_ $ do
        form_ [method_ "post", action_ "/placeBid"] $ do
          label_ [for_ "name"] "Name"
          input_ [type_ "text", id_ "name", name_ "name", required_ "required", pattern_ "\\S.*", title_ "non-whitespace text"]
          label_ [for_ "amount"] "Amount"
          input_ [type_ "number", id_ "amount", name_ "amount", min_ "0", step_ "1"]
          input_ [type_ "submit", value_ "Place bid"]

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