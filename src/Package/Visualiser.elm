module Package.Visualiser exposing (viewNode, viewVertex)

import Visualiser exposing (Msg(..), getNode)
import Package.Graph exposing (..)
import CustomSvg as G exposing (Svg)

type alias PosNode = Visualiser.PosNode Entity

getDesc : List PosNode -> String
getDesc nodes = "nodes: [ " ++ String.join ", " (List.map .id nodes) ++ " ]"

viewNode : PosNode -> Svg (Msg Entity)
viewNode node =
  let
    (x, y) = (node.x, node.y)
    text = G.text2 (x + 10) (y + 30) node.data.name
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

viewVertex : List PosNode -> Vertex -> Svg (Msg Entity)
viewVertex nodes vertex =
  case vertex.data of 
    Extends -> viewExtension nodes vertex
    References -> viewExtension nodes vertex
    Implements -> viewExtension nodes vertex

viewAttr : Float -> Float -> Attribute -> Float -> Svg (Msg Entity)
viewAttr x y attr offset =
    G.text1 (x + 10) (y + 30 + offset) <|
        attr.prettyTypeName ++ " " ++ attr.identifier

viewExtension : List PosNode -> Vertex -> Svg (Msg Entity)
viewExtension nodes vertex =
  Maybe.withDefault (G.group []) <|
    Maybe.map2
      (viewArrow G.arrow1)
      (getNode vertex.from nodes)
      (getNode vertex.to nodes)

viewReference : List PosNode -> Vertex -> Svg (Msg Entity)
viewReference nodes vertex =
  Maybe.withDefault (G.group [ ]) <|
    Maybe.map2
      (viewArrow G.arrow2)
      (getNode vertex.from nodes)
      (getNode vertex.to nodes)

viewArrow : ((Float, Float) -> (Float, Float) -> Svg (Msg Entity))
           -> PosNode -> PosNode -> Svg (Msg Entity)
viewArrow arrow from to =
  let
    moveTop (a, b) = (a + 70, b + 25)
    moveBottom (a, b) = (a + 70, b + 25)
    startPos = (\{x, y} -> moveTop (x, y)) from
    endPos = (\{x, y} -> moveBottom (x, y)) to
  in
    arrow startPos endPos
