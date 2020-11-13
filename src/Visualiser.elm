port module Visualiser exposing (Model, Msg, init, view, update, withGraph)

import Html exposing (Html)
import Html.Attributes
import Random
import Force
import CustomSvg as G exposing (Svg)
import Html.Events
import Graph exposing ( Graph, Entity, NodeId, Vertex
                      , Kind(..), Access(..), Attribute
                      )
import Config exposing (Config)

type alias Node =
  { data : Entity
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
  , references : List Vertex
  , draggedNode : Maybe Node
  }

type Msg
  = Start Node
  | Stop
  | Move (Float, Float)
  | ExportSvg

port exportSvg : () -> Cmd msg

init : Config -> Graph -> Model
init conf graph =
  { nodes = arrange conf (withRandomPositions graph.entities) graph.extensions
  , extensions = graph.extensions
  , implements = graph.implements
  , references = graph.references 
  , draggedNode = Nothing
  }

diff : List Node -> List Entity -> (List Node, List Entity)
diff old new =
  let
    newIds  = List.map .id new
    oldIds  = List.map .id old
    same    = List.filter (\node   ->      List.member node.id newIds   ) old
    changed = List.filter (\entity -> not (List.member entity.id oldIds)) new
  in
    (same, changed)

withGraph : Config -> Graph -> Model -> Model
withGraph conf graph model =
  let
    updatedNodes = List.map (updateNode graph.entities) model.nodes
    (keep, new) = diff updatedNodes graph.entities
    nodes = keep ++ arrange conf (withRandomPositions new) graph.extensions
  in 
    { nodes = nodes
    , extensions = graph.extensions
    , implements = graph.implements
    , references = graph.references 
    , draggedNode = Nothing
    }

updateNode : List Entity -> Node -> Node
updateNode entities node = 
  case List.filter (\{id} -> id == node.id) entities of
    match :: _ -> { node | data = match }
    []         -> node

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
    ExportSvg ->
      (model, exportSvg ())

view : Config -> Model -> Html Msg
view conf model =
  Html.div
    []
    [ viewOverlay model
    , G.render
        { move = Move
        , up = Stop
        , width = conf.width
        , height = conf.height
        }
        [ G.group
            [ G.group <| List.map (viewExtension model.nodes) model.extensions
            , G.group <| List.map (viewReference model.nodes) model.references
            , G.group <| List.map viewNode model.nodes
            ]
        ]
    ]

viewOverlay : Model -> Html Msg
viewOverlay model =
  Html.div
    [ Html.Attributes.id "overlay" ]
    [ Html.button
        [ Html.Events.onClick ExportSvg
        ]
        [ Html.text "Save SVG"
        ]
    , Html.text (getDesc model.nodes)
    ]

getDesc : List Node -> String
getDesc nodes = "nodes: [ " ++ String.join ", " (List.map .id nodes) ++ " ]"

viewNode : Node -> Svg Msg
viewNode node =
  let
    (x, y) = (node.x, node.y)
    text = G.text1 (x + 10) (y + 30) ("*" ++ node.data.name ++ "*")
    rect = case node.data.access of
             Public    -> G.rectClick2
             Private   -> G.rectClick1
             Protected -> G.rectClick1
    box = rect x y 120 (toFloat (50 + l * 20)) (Start node)
    l = List.length node.data.publicAttributes
    offsets = List.map (\a -> toFloat (a + 1) * 20) <| List.range 0 l
    attrs = List.map2 (viewAttr x y) node.data.publicAttributes offsets
  in
    G.group <| [ box, text ] ++ attrs

viewAttr : Float -> Float -> Attribute -> Float -> Svg Msg
viewAttr x y attr offset =
    G.text1 (x + 10) (y + 30 + offset) <|
        attr.prettyTypeName ++ " " ++ attr.identifier

viewExtension : List Node -> Vertex -> Svg Msg
viewExtension nodes vertex =
  Maybe.withDefault (G.group []) <|
    Maybe.map2
      (viewArrow G.arrow1)
      (getNode vertex.from nodes)
      (getNode vertex.to nodes)

viewReference : List Node -> Vertex -> Svg Msg
viewReference nodes vertex =
  Maybe.withDefault (G.group []) <|
    Maybe.map2
      (viewArrow G.arrow2)
      (getNode vertex.from nodes)
      (getNode vertex.to nodes)

viewArrow : ((Float, Float) -> (Float, Float) -> Svg Msg)
           -> Node -> Node -> Svg Msg
viewArrow arrow from to =
  let
    moveTop (a, b) = (a + 70, b + 25)
    moveBottom (a, b) = (a + 70, b + 25)
    startPos = (\{x, y} -> moveTop (x, y)) from
    endPos = (\{x, y} -> moveBottom (x, y)) to
  in
    arrow startPos endPos

getNode : NodeId -> List Node -> Maybe Node
getNode id nodes =
  case List.filter (\n -> n.data.id == id) nodes of
    node :: _ -> Just node
    [] -> Nothing

upsert : Node -> List Node -> List Node
upsert new nodes =
  case nodes of
    x::xs ->
      if x.data.id == new.data.id
        then new :: xs
        else x :: upsert new xs
    [] ->
      [new]

arrange : Config -> List Node -> List Vertex -> List Node
arrange config nodes extensions =
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
      [ Force.center (config.width / 2) (config.height / 2)
      , Force.customLinks 1 edges
      , Force.manyBodyStrength (-60) nodeIds
      ]
  in
    Force.computeSimulation (Force.simulation forces) nodes

wrap : Entity -> Float -> Float -> Node
wrap class x y =
  { data = class
  , x = x
  , y = y
  , vx = 0
  , vy = 0
  , id = class.id
  }

unique : List comparable -> Bool
unique l =
  case l of
    x::xs -> not (List.member x xs) && unique xs
    [] -> True

getRandomPositions : Int -> (List Float, List Float)
getRandomPositions num_ =
  let
    helper : Random.Seed -> Int -> (List Float, List Float)
    helper seed n =
      let
        ns = List.map toFloat (List.range 0 n)
        gen = Random.list n (Random.uniform 0 ns)
        (xs, seed2) = Random.step gen seed
        (ys, seed3) = Random.step gen seed2
      in
       if unique (List.map2 Tuple.pair xs ys)
         then (xs, ys)
         else helper seed3 n
   in
     helper (Random.initialSeed 1975) num_

withRandomPositions : List Entity -> List Node
withRandomPositions entities =
  let
    (xs, ys) = getRandomPositions (List.length entities)
  in
    List.map3 wrap entities xs ys
