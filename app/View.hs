{-# OPTIONS_GHC -Wno-orphans #-}

module View (withHead, bidForm) where

import Auction
import Data.Text (Text)
import Lucid

withHead :: Html () -> Html ()
withHead body =
  doctypehtml_ $ do
    head_ (title_ "Hask Ask")
    body_ $ do
      body
      script_ [src_ "/static/app.js", defer_ "defer"] ("" :: Text)

bidForm :: Html ()
bidForm =
  form_ [id_ "bid-form"] $ do
    label_ [for_ "name"] "Name"
    input_ [type_ "text", id_ "name", name_ "name", required_ "required", pattern_ "\\S.*", title_ "non-whitespace text"]
    label_ [for_ "amount"] "Amount"
    input_ [type_ "number", id_ "amount", name_ "amount", min_ "1", step_ "1"]
    div_ [id_ "msg"] mempty
    input_ [type_ "submit", value_ "Place bid"]

instance ToHtml Bid where
  toHtml bid =
    tr_ $ do
      td_ (toHtml $ name bid)
      td_ (toHtml $ show $ amount bid)

  toHtmlRaw = toHtml

instance ToHtml [Bid] where
  toHtml bids = table_ $ do
    tr_ $ do
      th_ "Name"
      th_ "Amount"
    foldMap toHtml bids

  toHtmlRaw = toHtml
