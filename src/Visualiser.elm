module Visualiser exposing (Model, Msg, init, view, update, withGraph)

import Html exposing (Html)
import Json.Decode as Decode

import Graph exposing (Graph, Class, NodeId, Vertex, Kind(..))
import CustomSvg as G exposing (Svg)

type alias Node =
  { data : Class
  , pos : Vec2
  , vel : Vec2
  }

type alias Vec2 = (Float, Float)

type alias Model =
  { nodes : List Node
  , extensions : List Vertex
  , implements : List Vertex
  , draggedNode : Maybe Node
  }

type Msg
  = Start Node
  | Stop
  | Move Vec2

init : Graph -> Model
init graph =
  let
    addPosAndVel class = { data = class, pos = (100, 100), vel = (0, 0) }
  in
    { nodes = arrange <| List.map addPosAndVel graph.classes
    , extensions = graph.extensions
    , implements = graph.implements
    , draggedNode = Nothing
    }

withGraph : Graph -> Model -> Model
withGraph graph model = init graph -- TODO

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start node ->
      ({ model | draggedNode = Just node }, Cmd.none)
    Stop ->
      ({ model | draggedNode = Nothing }, Cmd.none)
    Move (x, y) ->
      case model.draggedNode of
        Just old ->
          let
            new = { old | pos = (x - 60, y - 25) }
          in
            ({ model | nodes = upsert new model.nodes }, Cmd.none)
        Nothing ->
          (model, Cmd.none)

moveEventDecoder : Decode.Decoder Msg
moveEventDecoder =
  Decode.map Move <|
    Decode.map2 Tuple.pair
      (Decode.field "clientX" Decode.float)
      (Decode.field "clientY" Decode.float)

view : Model -> Html Msg
view model =
    G.render
      { move = moveEventDecoder
      , up = Decode.succeed Stop
      }
      [ G.group
          [ G.group <| List.map viewNode model.nodes
          , G.group <| List.map (viewVertex model.nodes) model.extensions
          ]
      ]

viewNode : Node -> Svg Msg
viewNode node =
  let
    (x, y) = node.pos
    text = G.text (x + 10) (y + 30) node.data.name
    rect = if node.data.kind == Normal then G.rectClick2 else G.rectClick1
    box = rect x y 120 50 (Decode.succeed (Start node))
  in
    G.group [ box, text ]

viewVertex : List Node -> Vertex -> Svg Msg
viewVertex nodes vertex =
  let
    moveTop (a, b) = (a + 70, b + 5)
    moveBottom (a, b) = (a + 70, b + 45)
    childPos = Maybe.map moveTop <| getPos nodes vertex.from
    parentPos = Maybe.map moveBottom <| getPos nodes vertex.to
  in
    case (childPos, parentPos) of
      (Just c, Just p) -> G.arrow1 c p
      _ -> G.group []

getPos : List Node -> NodeId -> Maybe Vec2
getPos nodes id =
  case List.filter (\n -> n.data.id == id) nodes of
    node :: _ -> Just node.pos
    [] -> Nothing

upsert : Node -> List Node -> List Node
upsert new nodes =
  case nodes of
    (x::xs) ->
      if x.data.id == new.data.id
        then new :: xs
        else x :: upsert new xs
    [] ->
      [new]

{-
zip : List a -> List b -> List (a, b)
zip xs ys =
  case (xs, ys) of
    (xHead::xTail, yHead::yTail) -> (xHead, yHead) :: zip xTail yTail
    (_, _) -> []
-}

-- ARRANGE

arrange : List Node -> List Node
arrange nodes = repeat 5 arrangeStep nodes

repeat : Int -> (a -> a) -> a -> a
repeat times f x =
  case times of
    0 -> x
    n -> repeat (n - 1) f (f x)

arrangeStep : List Node -> List Node
arrangeStep nodes = List.map (step nodes) nodes

step : List Node -> Node -> Node
step all node =
  let
    acc = getAccTotal node.pos others
    others = 
      all
      |> List.filter (\n -> n.data.id /= node.data.id)
      |> List.map (\n -> n.pos)
  in
    { data = node.data
    , pos = add node.pos (add node.vel acc)
    , vel = add node.vel acc
    }

add : Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

mul : Vec2 -> Vec2 -> Vec2
mul (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

getAccTotal : Vec2 -> List Vec2 -> Vec2
getAccTotal pos others =
    List.foldr add (0, 0) <| List.map (getAcc pos) others

getAcc : Vec2 -> Vec2 -> Vec2
getAcc (x1, y1) (x2, y2) =
  let
    l = ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) ^ 0.5
    r = 1
    k = 1
  in
    mul (l - r, l - r) (k, k)
