port module Visualiser exposing
  ( init, view, viewOverlay, update, tick, withConfig, withGraph, Model, Msg )

import Random
import Element exposing (Element)
import Element.Input
import Force
import CustomSvg
import Graph exposing (Graph)
import Config exposing (Config)
import Geometry as G exposing (Point, Rect)
import Instance exposing (Instance)

type alias PosNode n =
  { data : Graph.Node n
  , x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , id : Graph.NodeId
  }

type alias Model n e =
  { nodes : List (PosNode n)
  , edges : List (Graph.Edge e)
  , draggedNode : Maybe (PosNode n)
  , simulation : Force.State Graph.NodeId
  , scale : Float
  , movedBetweenClicks : Bool
  , instance : Instance n e (Msg n)
  }

type Msg n
  = Start (PosNode n)
  | Stop
  | Move (Float, Float)
  | ExportSvg
  | Scroll Float

port exportSvg : () -> Cmd msg

init : Config -> Graph n e -> Instance n e (Msg n) -> Model n e
init config graph instance =
  { nodes = withRandomPositions graph.nodes
  , edges = graph.edges |> List.filter (\{from, to} -> from /= to)
  , draggedNode = Nothing
  , simulation = getInitialSimulation
                   config instance (withRandomPositions graph.nodes) graph.edges
  , scale = 1
  , movedBetweenClicks = False
  , instance = instance
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

withGraph : Config -> Graph n e -> Model n e -> Model n e
withGraph config graph model =
  let
    updatedNodes = List.map (updateNode graph.nodes) model.nodes
    (keep, new) = diff updatedNodes graph.nodes
    newNodes = keep ++ withRandomPositions new
  in 
    { model
      | nodes = newNodes
      , edges = graph.edges |> List.filter (\{from, to} -> from /= to)
      , draggedNode = Nothing
      , simulation = getInitialSimulation config model.instance
                                   (withRandomPositions graph.nodes) graph.edges
    }

updateNode : List (Graph.Node n) -> PosNode n -> PosNode n
updateNode entities node = 
  case List.filter (\{id} -> id == node.id) entities of
    match :: _ -> { node | data = match }
    []         -> node

click : Instance n e (Msg n) -> PosNode n -> PosNode n
click { onClick } n =
  let
    data = n.data
    newData = { data | data = onClick n.data.data }
  in
    { n | data = newData }

update : Config -> Msg n -> Model n e -> (Model n e, Cmd (Msg n))
update config msg model =
  case msg of
    Start node ->
      ( { model
          | draggedNode = Just node
          , movedBetweenClicks = False
        }
      , Cmd.none
      )
    Stop ->
      ( case (model.movedBetweenClicks, model.draggedNode) of
        (False, Just node) ->
          let
            newNodes = upsert (click model.instance node) model.nodes 
          in
            { model
              | draggedNode = Nothing
              , nodes = newNodes
              , simulation = getInitialSimulation
                               config
                               model.instance
                               newNodes
                               model.edges
            }
        (True, Just node) ->
          { model
            | draggedNode = Nothing
            , simulation = Force.reheat model.simulation
          }
        (_, Nothing) ->
          model
      , Cmd.none
      )
    Move (x, y) ->
      ( { model
          | nodes =
            case model.draggedNode of
              Just old ->
                let
                  dx = (1 - model.scale) * config.width  * 0.5 / model.scale
                  dy = (1 - model.scale) * config.height * 0.5 / model.scale
                  mouse_x = x / model.scale - dx
                  mouse_y = y / model.scale - dy
                  new = { old | x = mouse_x, y = mouse_y }
                in
                  upsert new model.nodes
              Nothing ->
                model.nodes
          , movedBetweenClicks = True
        }
      , Cmd.none
      )
    ExportSvg ->
      (model, exportSvg ())
    Scroll deltaY ->
      ( let
          scale = clamp (0.2, 5) (model.scale + (deltaY / 500))
        in
          { model
            | scale = scale
          }  
      , Cmd.none
      )

withConfig : Config -> Model n e -> Model n e
withConfig config model =
  { model
    | simulation = getInitialSimulation
                     config
                     model.instance
                     model.nodes
                     model.edges
  }

clamp : (comparable, comparable) -> comparable -> comparable
clamp (min, max) value =
    if value <= min then
      min
    else if value >= max then
      max
    else
      value

view : Config -> Model n e -> CustomSvg.Svg (Msg n)
view config model =
  CustomSvg.render
    { move = Move
    , up = Stop
    , scroll = Scroll
    , width = config.width
    , height = config.height
    , scale = model.scale
    }
    [ CustomSvg.group
        [ CustomSvg.group
              (viewEdges model.instance model.nodes model.edges)
        , CustomSvg.group
              (viewNodes model.instance model.nodes)
        ]
    ]

viewEdges : Instance n e (Msg n) -> List (PosNode n) -> List (Graph.Edge e)
                                 -> List (CustomSvg.Svg (Msg n))
viewEdges { viewEdge, viewNode, getRect } nodeList edges =
  let
    viewEdge_ edge = 
      case (getNode nodeList edge.from, getNode nodeList edge.to) of
        (Just f, Just t) ->
            Just <| viewEdge
                        (f.data.data, getRect (G.point f.x f.y) f.data.data)
                        (t.data.data, getRect (G.point t.x t.y) t.data.data)
                        edge.data
        _ ->
            Nothing
  in
    List.filterMap viewEdge_ edges

viewNodes : Instance n e (Msg n) -> List (PosNode n)
                                 -> List (CustomSvg.Svg (Msg n))
viewNodes { viewNode, getRect } nodeList =
  List.map (\n -> viewNode
                    (Start n)
                    n.data.data
                    (getRect (G.point n.x n.y) n.data.data)
           ) nodeList

viewOverlay : Model n e -> Element (Msg n)
viewOverlay model =
  Element.column
    []
    [ Element.Input.button
        []
        { onPress = Just ExportSvg
        , label = Element.text "Save SVG"
        }
    , getInfo model
    ]

getInfo : Model n e -> Element (Msg n)
getInfo model =
  Element.column
    []
    [ Element.text <| String.fromInt (List.length model.nodes) ++
             " nodes & " ++ String.fromInt (List.length model.edges) ++ " edges"
             ++ " [" ++ String.join ", " (List.map .id model.nodes) ++ "]"
             ++ " [" ++ String.join ", " (List.map .from model.edges) ++ "]"
             ++ " [" ++ String.join ", " (List.map .to model.edges) ++ "]"
    , Element.text <| "(simulation" ++
                          if Force.isCompleted model.simulation
                            then " completed)"
                            else " calculating ...)"
    ]

getNode : List (PosNode n) -> Graph.NodeId -> Maybe (PosNode n)
getNode nodes id =
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

tick : Model n e -> Model n e
tick model =
  let
    (newState, newNodes) = Force.tick model.simulation model.nodes
  in
    { model
      | nodes = newNodes
      , simulation = newState
    }

getInitialSimulation : Config -> Instance n e (Msg n)
                              -> List (PosNode n)
                              -> List (Graph.Edge e)
                              -> Force.State Graph.NodeId
getInitialSimulation config { getRect } nodes edges =
  let
    edgesWithoutLoops =
      edges
      |> List.map (\{from, to} ->
                      { source = from
                      , target = to
                      , distance =
                          let fromR =
                                  from
                                  |> getNode nodes
                                  |> Maybe.map (\n -> getRect
                                                        (G.point n.x n.y)
                                                        n.data.data)
                                  |> Maybe.map G.radius
                                  |> Maybe.withDefault 0
                              toR =
                                  to
                                  |> getNode nodes
                                  |> Maybe.map (\n -> getRect
                                                        (G.point n.x n.y)
                                                        n.data.data)
                                  |> Maybe.map G.radius
                                  |> Maybe.withDefault 0
                          in fromR + 50 + toR
                      , strength = Nothing
                      }
                   )
    nodeIds = List.map .id nodes
    forces =
      [ Force.center (config.width / 2) (config.height / 2)
      , Force.customLinks 1 edgesWithoutLoops
      , Force.manyBodyStrength (-60) nodeIds
      ]
  in
    Force.simulation forces
