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
    Not -> Attrs
    Attrs -> Stats
    Stats -> Not

-- helpers

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

-- box

getRect : Point -> Entity -> Rect
getRect corner entity =
  let
    w = textWidth entity.name
    h = linesHeight (List.length attrs + 1)
    attrs = getInfo entity
    attrsW = attrs
             |> List.map textWidth
             |> List.maximum
             |> Maybe.withDefault 0
  in
    G.rect corner (max w attrsW) h

-- strings

getStats : Entity -> List String
getStats entity =
    let
        access =
            [ "access: " ++ case entity.access of
                Public -> "public"
                Protected -> "protected"
                Private -> "private"
            ]
        kind =
            [ "kind: " ++ case entity.kind of
                Class -> "class"
                Interface -> "interface"
                Enum -> "enum"
            ]
        details =
            let
                static =   if entity.static   then ["static"]   else []
                final =    if entity.final    then ["final"]    else []
                abstract = if entity.abstract then ["abstract"] else []
            in
            case (static ++ final ++ abstract) of
                [] -> []
                all -> [ "(" ++ String.join ", " all ++ ")" ]
        complexity =
            [ "complexity: " ++ (String.fromFloat entity.complexity) ]
    in
    List.concat
        [ kind
        , access
        , details
        , complexity
        ]

getInfo : Entity -> List String
getInfo entity =
  case entity.expansion of
    Not -> []
    Attrs -> getAttrs entity
    Stats -> getStats entity

getAttrs : Entity -> List String
getAttrs entity =
    case List.map attrToString entity.publicAttributes of
        [] -> [ "no public attributes" ]
        strings -> List.take 3 strings

attrToString : Attribute -> String
attrToString attr = attr.prettyTypeName ++ " " ++ attr.identifier

-- view

viewNode : x -> Entity -> Rect -> Svg x
viewNode msg entity rect =
  let
    text = S.text2 (G.move (G.corner rect) pad (charh + pad)) entity.name
    box =
        case entity.expansion of
            Not   -> S.rect1 rect
            Attrs -> S.rect2 rect
            Stats -> S.rect3 rect
    attrs = viewAttrs (G.corner rect) entity
  in
    S.click msg (S.group <| [ box, text ] ++ attrs)

viewAttrs : Point -> Entity -> List (Svg msg)
viewAttrs point entity =
  let
    attrs = getInfo entity
    offsets = List.map (\a -> toFloat (a + 1) * (charh + lineh)) <|
                List.range 0 (List.length attrs)
  in
    List.map2 (viewAttr point) attrs offsets

viewAttr : Point -> String -> Float -> Svg msg
viewAttr point attrName offset =
    S.text1 (G.move point pad (pad + charh + offset)) attrName

viewEdge : (Entity, Rect) -> (Entity, Rect) -> Link -> Svg x
viewEdge (_, fromRect) (_, toRect) link =
  case link of 
    Extends ->    S.arrow1 fromRect toRect
    References -> S.arrow2 fromRect toRect
    Implements -> S.arrow1 fromRect toRect

