{-# OPTIONS_GHC -Wno-orphans #-}

module View (withHead, bidForm, itemTableWithLink, itemTableWithoutLink) where

import Auction
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Data.Time 

withHead :: Html () -> Html ()
withHead body =
  doctypehtml_ $ do
    head_ (title_ "Hask Ask")
    body_ $ do
      body
      script_ [src_ "/static/app.js", defer_ "defer"] ("" :: Text)

itemTableWithLink :: [(Integer, ItemPure)] -> Html ()
itemTableWithLink items =
  table_ $ do
    thead_ $
      tr_ $ do
        th_ "Description"
        th_ "Ends at"
        th_ "Highest Bid"
        th_ "Status"
        th_ mempty
    tbody_ $
      mapM_ (\(itemId, item) -> addLinkToRow itemId $ itemRow item) items

itemTableWithoutLink :: [ItemPure] -> Html ()
itemTableWithoutLink items =
  table_ $ do
    thead_ $
      tr_ $ do
        th_ "Description"
        th_ "Ends at"
        th_ "Highest Bid"
        th_ "Status"
    tbody_ $
      mapM_ itemRow items

itemRow :: ItemPure -> Html ()
itemRow item = do
    td_ (toHtml $ description item)
    td_ (toHtml $ formatTime defaultTimeLocale "%D %R" $ endTime item)
    td_ $ case highestBid item of
      Nothing -> "—"
      Just bid -> toHtml (name bid <> " – " <> Text.pack (show (amount bid)))
    td_ (toHtml $ show $ runIdentity $ state item)

addLinkToRow :: Integer -> Html () -> Html ()
addLinkToRow itemId row = do
  row
  td_ $ a_ [href_ ("/item/" <> Text.pack (show itemId))] "View & Bid"

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
