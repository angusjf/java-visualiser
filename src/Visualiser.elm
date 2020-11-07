module Visualiser exposing (Model, Msg, init, view, update, withGraph)

import Html exposing (Html)
import Json.Decode as Decode
import Random
import Force
import Graph exposing (Graph, Class, NodeId, Vertex, Kind(..))
import CustomSvg as G exposing (Svg)

type alias Node =
  { data : Class
  , x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , id : NodeId
  }

type alias Model =
  { nodes : List Node
  , extensions : List Vertex
  , implements : List Vertex
  , draggedNode : Maybe Node
  }

type Msg
  = Start Node
  | Stop
  | Move (Float, Float)

wrap : Class -> Float -> Float -> Node
wrap class x y =
  { data = class
  , x = x
  , y = y
  , vx = 0
  , vy = 0
  , id = class.id
  }

init : Graph -> Model
init graph =
  let
    len = List.length graph.classes
    gen = Random.list len (Random.float -1 1)
    seed = Random.initialSeed 1975
    (xs, seed2) = Random.step gen seed
    (ys, _) = Random.step gen seed2
    nodes = List.map3 wrap graph.classes xs ys
  in
  { nodes = arrange nodes graph.extensions
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
            new =
              { old
              | x = x - 60
              , y = y - 25
              }
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
    (x, y) = (node.x, node.y)
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

getPos : List Node -> NodeId -> Maybe (Float, Float)
getPos nodes id =
  case List.filter (\n -> n.data.id == id) nodes of
    node :: _ -> Just (node.x, node.y)
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

arrange : List Node -> List Vertex -> List Node
arrange nodes extensions =
  let
    f {from, to} =
      { source = from
      , target = to
      , distance = 300
      , strength = Nothing
      }
    edges = List.map f extensions
    nodeIds = List.map .id nodes
    forces =
      [ Force.center 150 200
      , Force.customLinks 1 edges
      , Force.manyBodyStrength (-10) nodeIds
      ]
  in
    Force.computeSimulation (Force.simulation forces) nodes
