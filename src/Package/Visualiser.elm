module Package.Visualiser exposing (viewNode, viewEdge, getRect, onClick)

import Package.Graph exposing (..)
import CustomSvg as S exposing (Svg)
import Geometry as G exposing (Rect, Point)

onClick : Entity -> Entity
onClick entity =
  { entity | expansion = cycle entity.expansion }

cycle : Expansion -> Expansion
cycle e =
  case e of
    Not -> Fully
    Fully -> Not
    Half -> Fully

pad : Float
pad = 11

charh : Float
charh = 12

charw : Float
charw = 7.2

lineh : Float
lineh = 10

textWidth : String -> Float
textWidth str =
  pad * 2 + charw * (toFloat (String.length str))

linesHeight : Int -> Float
linesHeight n =
  pad * 2 + (toFloat n) * charh + (toFloat n - 1) * lineh

getRect : Point -> Entity -> Rect
getRect corner entity =
  let
    w = textWidth entity.name
    h = linesHeight (List.length attrs + 1)
    attrs = getAttrs entity
    attrsW = attrs
             |> List.map textWidth
             |> List.maximum
             |> Maybe.withDefault 0
  in
    G.rect corner (max w attrsW) h

viewNode : x -> Entity -> Rect -> Svg x
viewNode msg entity rect =
  let
    text = S.text2 (G.move (G.corner rect) pad (charh + pad)) entity.name
    box =
        case entity.access of
            Public    -> S.rect2 rect
            Private   -> S.rect1 rect
            Protected -> S.rect1 rect
    attrs = viewAttrs (G.corner rect) entity
  in
    S.click msg (S.group <| [ box, text ] ++ attrs)

viewAttrs : Point -> Entity -> List (Svg msg)
viewAttrs point entity =
  let
    attrs = getAttrs entity
    offsets = List.map (\a -> toFloat (a + 1) * (charh + lineh)) <|
                List.range 0 (List.length attrs)
    allAttrs = List.map2 (viewAttr point) attrs offsets
  in case entity.expansion of
    Not -> []
    Half -> List.take 3 <| allAttrs
    Fully -> allAttrs

getAttrs : Entity -> List String
getAttrs entity =
  let
    allAttrs = List.map attrToString entity.publicAttributes
  in case entity.expansion of
    Not -> []
    Half -> List.take 3 <| allAttrs
    Fully -> allAttrs

viewAttr : Point -> String -> Float -> Svg msg
viewAttr point attrName offset =
    S.text1 (G.move point pad (pad + charh + offset)) attrName

attrToString : Attribute -> String
attrToString attr = attr.prettyTypeName ++ " " ++ attr.identifier

viewEdge : (Entity, Rect) -> (Entity, Rect) -> Link -> Svg x
viewEdge (_, fromRect) (_, toRect) link =
  case link of 
    Extends ->    S.arrow1 fromRect toRect
    References -> S.arrow2 fromRect toRect
    Implements -> S.arrow1 fromRect toRect
