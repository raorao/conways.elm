module ConwowsGameOfLaughs where

import StartApp
import Effects
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, type', for, value, class)
import Dict exposing (Dict)
import Set exposing (Set)
import Time

type alias CoordinateX =
  Int

type alias CoordinateY =
  Int

type alias Board =
  Dict CoordinateX (Set CoordinateY)

type alias Model =
  { board: Board }

type Action
  = NoOp
  | GenerateNext


initialBoard : Board
initialBoard =
    Dict.fromList
      [ (3, (Set.fromList [3]) )
      , (4, (Set.fromList [4]) )
      , (5, (Set.fromList [2,3,4]) )
      ]


initialModel : Model
initialModel =
  { board = initialBoard }

update action model =
  case action of
    GenerateNext ->
      let
        newBoard =
          generateNextBoard model.board

        newModel =
          { model | board <- newBoard }

      in
      (newModel, Effects.none)

    NoOp ->
      (model, Effects.none)

generateSignal =
  Time.every 100
    |> Signal.map (always GenerateNext)

app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [generateSignal]
    }

main =
  app.html


init : (Model, Effects.Effects action)
init =
  (initialModel, Effects.none)

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
        [0..30]
          |> List.map (generateCell row)
          |> tr [class "row"]

    generateRows =
      [0..30]
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

type alias CounterBoard =
  Dict CoordinateX (Dict CoordinateY Int)

generateNeighborIndeces : CoordinateX -> CoordinateY -> List (CoordinateX, CoordinateY)
generateNeighborIndeces x y =
  let
    offsets =
      [ (-1,-1)
      , (-1, 0)
      , (-1, 1)
      , (0, -1)
      , (0, 1)
      , (1,-1)
      , (1, 0)
      , (1, 1)
      ]
  in
    offsets
      |> List.map (\(offsetX,offsetY) -> ( (offsetX + x), (offsetY + y) ) )

(?) = flip Maybe.withDefault

generateCounterBoard : Board -> CounterBoard
generateCounterBoard board =
  let
    updateCellCounts : CoordinateY -> Maybe (Dict CoordinateY Int) -> Maybe (Dict CoordinateY Int)
    updateCellCounts y maybeDict =
        maybeDict
          |> Maybe.withDefault (Dict.singleton y 0)
          |> Dict.update y (\maybeY -> Just ((maybeY ? 0) + 1))
          |> Just

    flagNeighbor : (CoordinateX, CoordinateY) -> CounterBoard -> CounterBoard
    flagNeighbor (x,y) initialCounts =
      Dict.update x (updateCellCounts y) initialCounts

    flagNeighborsForCell : CoordinateX -> CoordinateY -> CounterBoard -> CounterBoard
    flagNeighborsForCell x y initialCounts =
      let
        neighborIndeces =
          generateNeighborIndeces x y
      in
        List.foldr flagNeighbor initialCounts neighborIndeces

    flagNeighborsForRow : CoordinateX -> (Set CoordinateY) -> CounterBoard -> CounterBoard
    flagNeighborsForRow xIndex row initialCounts =
      Set.foldr (flagNeighborsForCell xIndex) initialCounts row

  in
    Dict.foldr flagNeighborsForRow Dict.empty board



generateNextBoard : Board -> Board
generateNextBoard board =
  let
    checkIsAlive : Set CoordinateY -> (CoordinateY, Int) -> Maybe CoordinateY
    checkIsAlive aliveYs (y, neighborCount) =
      if Set.member y aliveYs then
        if neighborCount == 2 || neighborCount == 3 then
          Just y
        else
          Nothing
      else
        if neighborCount == 3 then
          Just y
        else
          Nothing


    counterBoard =
      generateCounterBoard board


    updateRow : CoordinateX -> Set CoordinateY -> Set CoordinateY
    updateRow x aliveYs =
      counterBoard
        |> Dict.get x
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap (checkIsAlive aliveYs)
        |> Set.fromList

    expandedBoard =
      let
        currentXs =
          Dict.keys board

        maxX =
          List.maximum currentXs
            |> Maybe.withDefault 0

        minX =
          List.minimum currentXs
            |> Maybe.withDefault 0
      in
        board
          |> Dict.insert (maxX + 1) Set.empty
          |> Dict.insert (minX - 1) Set.empty


  in
    Dict.map updateRow expandedBoard