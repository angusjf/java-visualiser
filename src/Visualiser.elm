port module Visualiser exposing
  ( init, view, viewOverlay, update, tick, withConfig, withGraph, Model, Msg )

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
  , static : Bool
  }

type Msg n
  = Start (PosNode n)
  | Stop
  | Move (Float, Float)
  | ExportSvg
  | Scroll Float
  | SetStatic Bool
  | SnapToCircle

port exportSvg : () -> Cmd msg

withoutSelfLoops : List (Graph.Edge e) -> List (Graph.Edge e)
withoutSelfLoops = List.filter (\{to, from} -> to /= from)

init : Config -> Graph n e -> Instance n e (Msg n) -> Model n e
init config graph instance =
  let
    posNodes = withPositions config.width config.height graph.nodes
    noLoops = withoutSelfLoops graph.edges
    static = False
  in
    { nodes = posNodes
    , edges = noLoops
    , draggedNode = Nothing
    , simulation = getInitialSimulation config static instance posNodes noLoops
    , scale = 1
    , movedBetweenClicks = False
    , instance = instance
    , static = static
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
    newNodes = keep ++ withPositions config.width config.height new
    noLoops = withoutSelfLoops graph.edges
  in 
    { model
      | nodes = newNodes
      , edges = noLoops
      , draggedNode = Nothing
      , simulation = getInitialSimulation config model.static
                                              model.instance newNodes noLoops
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
            s = getInitialSimulation config model.static
                                           model.instance newNodes model.edges
          in
            { model
              | draggedNode = Nothing
              , nodes = newNodes
              , simulation = s
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
    SetStatic bool ->
      ( { model
          | static = bool
          , simulation = getInitialSimulation
                            config
                            bool
                            model.instance
                            model.nodes
                            model.edges
        }
      , Cmd.none
      )
    SnapToCircle ->
      let
        oldNodes = List.map .data model.nodes
        newNodes = withPositions config.width config.height oldNodes
      in
        ( { model
            | nodes = newNodes
            , simulation = getInitialSimulation
                              config
                              model.static
                              model.instance
                              newNodes
                              model.edges
          }
        , Cmd.none
        )

withConfig : Config -> Model n e -> Model n e
withConfig config model =
  { model
    | simulation = getInitialSimulation
                     config
                     model.static
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
        [] <|
        if model.static then
            { onPress = Just (SetStatic False)
            , label = Element.text "Enable Forces"
            }
        else
            { onPress = Just (SetStatic True)
            , label = Element.text "Disable Forces"
            }
    , Element.Input.button
        []
        { onPress = Just SnapToCircle
        , label = Element.text "Reset Positions"
        }
    , Element.Input.button
        []
        { onPress = Just ExportSvg
        , label = Element.text "Save as SVG"
        }
    ]

getInfo : Model n e -> Element (Msg n)
getInfo model =
  Element.column
    []
    [ Element.text <| String.fromInt (List.length model.nodes) ++
             " nodes & " ++ String.fromInt (List.length model.edges) ++ " edges"
             --++ " [" ++ String.join ", " (List.map .id model.nodes) ++ "]"
             --++ " [" ++ String.join ", " (List.map .from model.edges) ++ "]"
             --++ " [" ++ String.join ", " (List.map .to model.edges) ++ "]"
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

withPositions : Float -> Float -> List (Graph.Node n) -> List (PosNode n)
withPositions width height entities =
  let
    num = List.length entities
    xs = List.map (\t -> a * width * sin t + width / 2) ts
    ys = List.map (\t -> a * height * cos t + height / 2) ts
    angle = (2 * pi) / toFloat num
    ts = List.map (\t -> angle * toFloat t) <| List.range 0 (num - 1)
    a = 0.3
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

getInitialSimulation : Config -> Bool
                              -> Instance n e (Msg n)
                              -> List (PosNode n)
                              -> List (Graph.Edge e)
                              -> Force.State Graph.NodeId
getInitialSimulation config static { getRect } nodes edges =
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
      if static then
        [
        ]
      else
        [ Force.center (config.width / 2) (config.height / 2)
        , Force.customLinks 1 edgesWithoutLoops
        , Force.manyBodyStrength (-60) nodeIds
        ]
  in
    Force.simulation forces
