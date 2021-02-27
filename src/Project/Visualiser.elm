module Project.Visualiser exposing (viewNode, viewEdge, getRect, onClick)

import Project.Graph exposing (..)
import CustomSvg as S exposing (Svg)
import Geometry as G exposing (Rect, Point)

onClick : Entity -> Entity
onClick = identity

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

getTag : String -> String
getTag name = if name == "" then "default package" else name

getRect : Point -> Entity -> Rect
getRect corner entity =
  let
    tag = getTag entity.name
    w = textWidth tag
    h = linesHeight 1
  in
    G.rect corner w h

viewNode : x -> Entity -> Rect -> Svg x
viewNode msg entity rect =
  let
    tag = getTag entity.name
    text = S.text2 (G.move (G.corner rect) pad (charh + pad)) tag
    box =
        case entity.kind of
            Class     -> S.rect2 rect
            Package   -> S.rect1 rect
  in
    S.click msg (S.group [ box, text ])

viewEdge : (Entity, Rect) -> (Entity, Rect) -> Link -> Svg x
viewEdge (_, fromRect) (_, toRect) link =
  S.arrow2 fromRect toRect
