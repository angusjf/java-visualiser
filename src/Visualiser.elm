module Visualiser exposing (Model, Msg, init, view, update, withGraph)

import Html exposing (Html)
import Json.Decode as Decode

import Graph exposing (Graph, Class, NodeId)
import CustomSvg as G exposing (Svg)

type alias Model =
  { graph : Graph
  , positions : List (Class, (Float, Float))
  , draggedClass : Maybe NodeId
  }

type Msg
  = Start NodeId
  | Stop
  | Move DragData

type alias DragData =
  { x : Float
  , y : Float
  }

init : Graph -> Model
init graph =
  let
    l = List.length graph.classes
    xs = List.map (\x -> (x ^ 2.2) * 2 + 10) <| flip <| List.map toFloat <| List.range 0 l
    ys = List.map (\y -> y * 60 + 10) <| List.map toFloat <| List.range 0 l
    poss = zip3With (\c x y -> (c, (x, y))) graph.classes xs ys
  in
    { graph = graph
    , positions = poss
    , draggedClass = Nothing
    }

withGraph : Graph -> Model -> Model
withGraph graph model = init graph
--withGraph graph model =
--  { model | graph = graph
--          , positions = 
--  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start id ->
      ({ model | draggedClass = Just id }, Cmd.none)
    Stop ->
      ({ model | draggedClass = Nothing }, Cmd.none)
    Move dragData ->
      case model.draggedClass of
        Just draggedId ->
          let
            new = List.map up model.positions
            up (class, (x, y)) =
              if class.id == draggedId
                then (class, (dragData.x - 60, dragData.y - 25))
                else (class, (x, y))
          in
            ({ model | positions = new }, Cmd.none)
        Nothing -> (model, Cmd.none)

moveEventDecoder : Decode.Decoder Msg
moveEventDecoder =
  Decode.map Move <|
    Decode.map2 DragData
      (Decode.field "clientX" Decode.float)
      (Decode.field "clientY" Decode.float)

eventDecoder : Msg -> Decode.Decoder Msg
eventDecoder msg = Decode.succeed msg

flip : List a -> List a
flip list =
  let
    flip_ i l =
      case l of 
        x::y::xs ->
          if modBy 2 i == 0
            then x :: y :: List.reverse (flip_ (i + 1) xs)
            else y :: x :: (flip_ (i + 1) xs)
        smth -> smth
    noflip_ _ l = l
  in
    flip_ 0 list

view : Model -> Html Msg
view model =
    G.render
      { move = moveEventDecoder
      , up = eventDecoder Stop
      }
      [ G.group
          [ G.group <| List.map viewClass model.positions
          , G.group <| List.map (viewExtension model.positions) model.graph.classes
          ]
      ]

viewClass : (Class, (Float, Float)) -> Svg Msg
viewClass (class, (x, y)) =
  let
    text = G.text (x + 10) (y + 30) class.name
    rect = if class.public then G.rectClick2 else G.rectClick1
    box = rect x y 120 50 (eventDecoder (Start class.id))
  in
    G.group [ box, text ]

viewExtension : List (Class, (Float, Float)) -> Class -> Svg Msg
viewExtension poss class =
  let
    childPos_ = getPos poss class.id
    parentPos_ = Maybe.andThen (getPos poss) class.extends
    childPos = Maybe.map (\(a, b) -> (a + 70, b + 5)) childPos_
    parentPos = Maybe.map (\(a, b) -> (a + 70, b + 45)) parentPos_
  in
    case (childPos, parentPos) of
      (Just c, Just p) -> G.arrow1 c p
      _ -> G.group []

getPos : List (Class, (Float, Float)) -> NodeId -> Maybe (Float, Float)
getPos poss id =
  case List.filter (\(class, (x, y)) -> class.id == id) poss of
    (_, (x, y)) ::_ -> Just (x, y)
    [] -> Nothing

zip3With : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zip3With f aList bList cList =
  case (aList, bList, cList) of
    (a::aa, b::bb, c::cc) -> (f a b c) :: zip3With f aa bb cc
    _ -> []
