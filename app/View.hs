{-# OPTIONS_GHC -Wno-orphans #-}

module View
  ( withHead,
    bidForm,
    itemTableWithLink,
    itemTableWithoutLink,
    itemForm,
    itemRow,
    itemLink,
  )
where

import Auction
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Lucid

withHead :: Text -> Html () -> Html ()
withHead jsSource body =
  doctypehtml_ $ do
    head_ (title_ "Hask Ask")
    body_ $ do
      body
      script_ [src_ jsSource, type_ "module"] ("" :: Text)

itemTableWithLink :: UUID -> [(Integer, UUID, ItemPure)] -> Html ()
itemTableWithLink itemListSubscrId items =
  table_ [id_ $ UUID.toText itemListSubscrId] $ do
    thead_ $
      tr_ $ do
        th_ "Description"
        th_ "Asking price"
        th_ "Ends at"
        th_ "Highest Bid"
        th_ "Status"
        th_ mempty
    tbody_ $
      mapM_ (\(itemId, subscriptionId, item) -> itemRow (Just $ itemLink itemId) item subscriptionId) items

itemTableWithoutLink :: [(ItemPure, UUID)] -> Html ()
itemTableWithoutLink items =
  table_ $ do
    thead_ $
      tr_ $ do
        th_ "Description"
        th_ "Asking price"
        th_ "Ends at"
        th_ "Highest Bid"
        th_ "Status"
    tbody_ $
      mapM_ (uncurry $ itemRow Nothing) items

itemRow :: Maybe (Html ()) -> ItemPure -> UUID -> Html ()
itemRow maybeLink item subscriptionId = do
  let row = do
        td_ [name_ "description"] (toHtml $ description item)
        td_ [name_ "askingPrice"] (toHtml $ show $ askingPrice item)
        td_ [name_ "endTime"] (toHtml $ formatTime defaultTimeLocale "%D %R" $ endTime item)
        td_ [name_ "highestBid"] $
          case highestBid item of
            Nothing -> "—"
            Just bid -> toHtml (name bid <> " – " <> Text.pack (show (amount bid)))
        td_ [name_ "state"] (toHtml $ show $ runIdentity $ state item)
  case maybeLink of
    Nothing -> tr_ [id_ $ UUID.toText subscriptionId] row
    Just link -> tr_ [id_ $ UUID.toText subscriptionId] $ row <> link

itemLink :: Integer -> Html ()
itemLink itemId =
  td_ $ a_ [href_ ("/item/" <> Text.pack (show itemId))] "View & Bid"

itemForm :: Html ()
itemForm =
  form_ [id_ "item-form"] $ do
    label_ [for_ "description"] "Description"
    input_ [type_ "text", id_ "description", name_ "description", required_ "required", pattern_ "\\S.*", title_ "non-whitespace text"]
    label_ [for_ "askingPrice"] "Asking price"
    input_ [type_ "number", id_ "askingPrice", name_ "askingPrice", min_ "1", step_ "1"]
    div_ [id_ "item-msg"] mempty
    input_ [type_ "submit", value_ "Sell item"]

bidForm :: Html ()
bidForm =
  form_ [id_ "bid-form"] $ do
    label_ [for_ "name"] "Name"
    input_ [type_ "text", id_ "name", name_ "name", required_ "required", pattern_ "\\S.*", title_ "non-whitespace text"]
    label_ [for_ "amount"] "Amount"
    input_ [type_ "number", id_ "amount", name_ "amount", min_ "1", step_ "1"]
    div_ [id_ "bid-msg"] mempty
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
