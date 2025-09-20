module View where

import Lucid

withHead :: Html () -> Html ()
withHead body =
  doctypehtml_ $ do
      head_ (title_ "Hask Ask")
      body_ body

bidForm :: Html ()
bidForm =
  form_ [method_ "post", action_ "/placeBid"] $ do
    label_ [for_ "name"] "Name"
    input_ [type_ "text", id_ "name", name_ "name", required_ "required", pattern_ "\\S.*", title_ "non-whitespace text"]
    label_ [for_ "amount"] "Amount"
    input_ [type_ "number", id_ "amount", name_ "amount", min_ "0", step_ "1"]
    input_ [type_ "submit", value_ "Place bid"]