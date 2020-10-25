module Visualiser exposing (Model, Msg, init, view, update, setGraph)

import Html exposing (Html)

import Graph exposing (Graph, Class, NodeId)
import CustomSvg as G exposing (Svg)

type alias Model =
  { graph : Graph
  }

type alias Msg = ()

init : Graph -> Model
init graph = { graph = graph
             }

setGraph : Graph -> Model -> Model
setGraph graph model = { model | graph = graph }

view : Model -> Html Msg
view model =
  let
    graph = model.graph
    l = List.length graph.classes
    xs = List.map (\x -> (x ^ 2.2) * 2) <| flip <| List.map toFloat <| List.range 0 l
    ys = List.map (\y -> y * 60) <| List.map toFloat <| List.range 0 l
    classPoss = zip3 xs ys graph.classes
    extPoss = zip3 xs ys graph.extensions
  in
    G.render
      [
        G.group
          [ G.group <| List.map viewClass classPoss
          , G.group <| List.map (viewExtension classPoss) extPoss
          ]
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update dragEvent model = (model, Cmd.none)

--   Svg.Events.on "click" (decodeClickEvent 
-- 
--{- target: h1.title, buttons: 0, clientX: 392, clientY: 131, layerX: 392,
--   layerY: 131 -}
--decodeClickEvent : Json.Decode.Decoder Msg
--decodeClickEvent nodeId =
--  Decode.succeed
--    { x = 1
--    , y = 1
--    , id = nodeId
--    }

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

viewClass : (Float, Float, Class) -> Svg Msg
viewClass (x, y, class) =
  let
    text = G.text (x + 10) (y + 30) class.name
    box = (if class.public then G.rect2 else G.rect1) x y 120 50
  in
    G.group [ box, text ]

viewExtension : List (Float, Float, Class) -> (Float, Float, (NodeId, NodeId)) -> Svg Msg
viewExtension classPoss (x, y, (child, parent)) =
  let
    childPos = Maybe.map (\(a, b) -> (a + 70, b + 30)) <| getPos classPoss child
    parentPos = Maybe.map (\(a, b) -> (a + 70, b + 30)) <| getPos classPoss parent
  in
    case (childPos, parentPos) of
      (Just c, Just p) -> G.arrow1 c p
      _ -> G.group []

getPos : List (Float, Float, Class) -> NodeId -> Maybe (Float, Float)
getPos poss id =
  case List.filter (\(x, y, class) -> class.id == id) poss of
    (x, y, _)::_ -> Just (x, y)
    [] -> Nothing

zip3 : List a -> List b -> List c -> List (a, b, c)
zip3 aList bList cList =
  case (aList, bList, cList) of
    (a::aa, b::bb, c::cc) -> (a, b, c) :: zip3 aa bb cc
    _ -> []
