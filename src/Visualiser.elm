port module Visualiser exposing (..)

import Random
import Element exposing (Element)
import Element.Input
import Force
import CustomSvg
import Graph exposing (Graph)
import Config exposing (Config)

type alias PosNode n =
  { data : Graph.Node n
  , x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , id : Graph.NodeId
  }

type alias Model n v =
  { nodes : List (PosNode n)
  , vertices : List (Graph.Vertex v)
  , draggedNode : Maybe (PosNode n)
  , simulation : Force.State Graph.NodeId
  , viewNode : NodeViewer n
  , viewVertex : VertexViewer n v
  }

type alias NodeViewer n =
   PosNode n -> CustomSvg.Svg (Msg n)

type alias VertexViewer n v =
   List (PosNode n) -> Graph.Vertex v -> CustomSvg.Svg (Msg n)

type Msg n
  = Start (PosNode n)
  | Stop
  | Move (Float, Float)
  | ExportSvg

port exportSvg : () -> Cmd msg

init : Config -> Graph n v -> NodeViewer n -> VertexViewer n v -> Model n v
init config graph viewNode viewVertex =
  { nodes = withRandomPositions graph.nodes
  , vertices = graph.vertices 
  , draggedNode = Nothing
  , simulation = getInitialSimulation
                   config (withRandomPositions graph.nodes) graph.vertices
  , viewNode = viewNode
  , viewVertex = viewVertex
  }

diff : List (PosNode n) -> List (Graph.Node n)
                        -> (List (PosNode n), List (Graph.Node n))
diff old new =
  let
    newIds  = List.map .id new
    oldIds  = List.map .id old
    same    = List.filter (\node   ->      List.member node.id newIds   ) old
    changed = List.filter (\entity -> not (List.member entity.id oldIds)) new
  in
    (same, changed)

withGraph : Config -> Graph n v -> Model n v -> Model n v
withGraph config graph model =
  let
    updatedNodes = List.map (updateNode graph.nodes) model.nodes
    (keep, new) = diff updatedNodes graph.nodes
    nodes = keep ++ withRandomPositions new
  in 
    { model
      | nodes = nodes
      , vertices = graph.vertices 
      , draggedNode = Nothing
      , simulation = getInitialSimulation
                      config (withRandomPositions graph.nodes) graph.vertices
    }

updateNode : List (Graph.Node n) -> PosNode n -> PosNode n
updateNode entities node = 
  case List.filter (\{id} -> id == node.id) entities of
    match :: _ -> { node | data = match }
    []         -> node

update : Msg n -> Model n v -> (Model n v, Cmd (Msg n))
update msg model =
  case msg of
    Start node ->
      ({ model | draggedNode = Just node }, Cmd.none)
    Stop ->
      ( { model
          | draggedNode = Nothing
          , simulation = Force.reheat model.simulation
        }
      , Cmd.none
      )
    Move (x, y) ->
      case model.draggedNode of
        Just old ->
          let
            new = { old | x = x - 60 , y = y - 25 }
          in
            ({ model | nodes = upsert new model.nodes }, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    ExportSvg ->
      (model, exportSvg ())

view : Config -> Model n v -> Element (Msg n)
view config model =
  Element.column
    [ ]
    [ viewOverlay model
    , Element.html <| CustomSvg.render
        { move = Move
        , up = Stop
        , width = config.width
        , height = config.height
        }
        [ CustomSvg.group
            [ CustomSvg.group
                (List.map (model.viewVertex model.nodes) model.vertices)
            , CustomSvg.group
                (List.map model.viewNode model.nodes)
            ]
        ]
    ]

viewOverlay : Model n v -> Element (Msg n)
viewOverlay model =
  Element.row
    []
    [ Element.Input.button
        []
        { onPress = Just ExportSvg
        , label = Element.text "Save SVG"
        }
    , Element.text (getInfo model)
    ]

getInfo : Model n v -> String
getInfo model =
  " " ++ String.fromInt (List.length model.nodes) ++ " nodes & " ++ 
  String.fromInt (List.length model.vertices) ++ " vertices (simulation" ++
  if Force.isCompleted model.simulation
    then " completed)"
    else " calculating ...)"


getNode : Graph.NodeId -> List (PosNode n) -> Maybe (PosNode n)
getNode id nodes =
  case List.filter (\n -> n.data.id == id) nodes of
    node :: _ -> Just node
    [] -> Nothing

upsert : PosNode n -> List (PosNode n) -> List (PosNode n)
upsert new nodes =
  case nodes of
    x::xs ->
      if x.data.id == new.data.id
        then new :: xs
        else x :: upsert new xs
    [] ->
      [new]

wrap : Graph.Node n -> Float -> Float -> PosNode n
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

withRandomPositions : List (Graph.Node n) -> List (PosNode n)
withRandomPositions entities =
  let
    (xs, ys) = getRandomPositions (List.length entities)
  in
    List.map3 wrap entities xs ys

tick : Model n v -> Model n v
tick model =
  let
    (newState, newNodes) = Force.tick model.simulation model.nodes
  in
    { model
      | nodes = newNodes
      , simulation = newState
    }

arrange : Config -> List (PosNode n) -> List (Graph.Vertex v) -> List (PosNode n)
arrange config nodes extensions =
  Force.computeSimulation (getInitialSimulation config nodes extensions) nodes

getInitialSimulation : Config -> List (PosNode n) -> List (Graph.Vertex v)
                                                  -> Force.State Graph.NodeId
getInitialSimulation config nodes extensions =
  let
    edges =
      extensions
      |> List.map (\{from, to} ->
                      { source = from
                      , target = to
                      , distance = 200
                      , strength = Nothing
                      }
                   )
    nodeIds = List.map .id nodes
    forces =
      [ Force.center (config.width / 2) (config.height / 2)
      , Force.customLinks 1 edges
      , Force.manyBodyStrength (-60) nodeIds
      ]
  in
    Force.simulation forces
