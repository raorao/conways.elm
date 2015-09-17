module ConwowsGameOfLaughs where

import StartApp
import Effects
import Html exposing (..)
import Html.Attributes exposing (id, type', for, value, class)
import Dict exposing (Dict)
import Set exposing (Set)

type alias CoordinateX =
  Int

type alias CoordinateY =
  Int

type alias Board =
  Dict CoordinateX (Set CoordinateY)

type alias Model =
  { board: Board }


initialBoard : Board
initialBoard =
    Dict.fromList
      [ (0, (Set.fromList [0, 1]) ) ]


initialModel : Model
initialModel =
  { board = initialBoard }


update action model =
  (model, Effects.none)


app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }

main =
  app.html


init : (Model, Effects.Effects action)
init =
  (initialModel, Effects.none)

view : a -> Model -> Html
view actionDispatcher model =
  let
    foo = "bar"

    generateCell : Set CoordinateY -> CoordinateY -> Html
    generateCell row yIndex =
      let
        cellClass =
          if Set.member yIndex row then
            "alive"
          else
            "dead"
      in
        td [class cellClass] []

    generateRow : Maybe (Set CoordinateY) -> Html
    generateRow maybeRow =
      let
        row =
          case maybeRow of
            Just row -> row
            Nothing  -> Set.empty
      in
        [0..7]
          |> List.map (generateCell row)
          |> tr [class "row"]

    generateRows =
      [0..7]
        |> List.map (flip Dict.get model.board)
        |> List.map generateRow
        |> table [class "board"]

  in
    div
      [ class "container" ]
      [ h1 [] [text "Tree of Elm Life Dot Biz"]
      , h2 [] [text "its gunna be a billyun dollar company - Srinivas The Great"]
      , generateRows
      ]

