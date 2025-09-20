{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api (runApp) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

data Bid = Bid
  { name :: Text,
    amount :: Int
  }
  deriving (Generic)

instance ToJSON Bid

instance FromJSON Bid

-- instance ToHtml Bid where
--   toHtml bid =
--     tr_ $
--       td_ (toHtml $ name bid) <> td_ (toHtml $ show $ amount bid)

--   toHtmlRaw = toHtml

-- instance ToHtml [Bid] where
--   toHtml bids = table_ $ do
--     tr_ $ do
--       th_ "Name"
--       th_ "Amount"
--     foldMap toHtml bids

--   toHtmlRaw = toHtml

bids :: [Bid]
bids =
  [ Bid "Elli" 500,
    Bid "Håkon" 600
  ]

type API =
  Get '[HTML] (Html ())
    :<|> "allBids" :> Get '[JSON] [Bid]
    :<|> "placeBid" :> ReqBody '[JSON] Bid :> Post '[HTML] (Html ())

getHome :: Handler (Html ())
getHome =
  return $
    doctypehtml_ $ do
      head_ (title_ "Hask Ask")
      body_ $ do
        form_ [method_ "post", action_ "/placeBid"] $ do
          label_ [for_ "name"] "Name"
          input_ [type_ "text", id_ "name", name_ "name"]
          label_ [for_ "amount"] "Amount"
          input_ [type_ "text", id_ "amount", name_ "amount"]
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