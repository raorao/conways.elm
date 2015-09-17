module ConwowsGameOfLaughs where

import StartApp
import Effects
import Html exposing (..)
import Html.Attributes exposing (id, type', for, value, class)

initialModel =
  { board = [] }

init =
  (initialModel, Effects.none)

update action model =
  (model, Effects.none)

view actionDispatcher model =
  div
    [ class "container" ]
    [ h1 [] [text "Tree of Elm Life Dot Biz"]
    , h2 [] [text "its gunna be a billyun dollar company - Srinivas The Great"]
    ]

app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }

main =
  app.html