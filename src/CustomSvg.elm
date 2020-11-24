module CustomSvg exposing (..)

import Svg
import TypedSvg as S
import TypedSvg.Attributes as A
import TypedSvg.Events as E
import TypedSvg.Types as T
import Json.Decode
import VirtualDom
import Svg.Attributes as UntypedA
import VsColor exposing (VsColor(..), vsColor)
import Geometry as G exposing (Point, Rect)

type alias Svg a = Svg.Svg a

render : { width : Float
         , height : Float
         , move : (Float, Float) -> msg
         , up : msg
         , scroll : Float -> msg
         , scale : Float
         }
      -> List (Svg msg) -> Svg msg
render opts things =
  S.svg
    [ A.width  <| T.px <| opts.width  * (1 / opts.scale)
    , A.height <| T.px <| opts.height * (1 / opts.scale)
    , E.on "mousemove" (moveEventDecoder opts.move)
    , E.onMouseUp opts.up
    , E.on "wheel" (wheelEventDecoder opts.scroll)
    , A.transform [ T.Scale opts.scale opts.scale ]
    ]
    [ S.defs
      []
      [ S.marker
        [ A.id "arrowHeadFill"
        , A.viewBox 0 0 10 10
        , A.refX "1"
        , A.refY "5"
        , UntypedA.markerUnits "strokeWidth"
        , UntypedA.markerWidth "10"
        , UntypedA.markerHeight "10"
        , UntypedA.orient "auto"
        ]
        [ S.path
          [ A.d "M 0 0 L 10 5 L 0 10"
          , UntypedA.fill (vsColor White)
          ] []
        ]
      , S.marker
        [ A.id "arrowHeadLine"
        , A.viewBox 0 0 10 10
        , A.refX "1"
        , A.refY "3"
        , UntypedA.markerUnits "strokeWidth"
        , UntypedA.markerWidth "10"
        , UntypedA.markerHeight "10"
        , UntypedA.orient "auto"
        ]
        [ S.path
          [ A.d "M 0 0 L 6 3 L 0 6"
          , UntypedA.stroke (vsColor White)
          , UntypedA.fill "none"
          ] []
        ]
      ]
    , S.g [] <|
        ( S.rect
          [ UntypedA.width "100%"
          , UntypedA.height "100%"
          , UntypedA.fill (vsColor Background)
          ]
          []
        ) :: things
    ]

moveEventDecoder : ((Float, Float) -> msg) -> VirtualDom.Handler msg
moveEventDecoder msg =
  VirtualDom.Custom <|
    Json.Decode.map (\m -> { message = msg m
                           , preventDefault = True
                           , stopPropagation = True
                           }
                    ) <|
      Json.Decode.map2 Tuple.pair
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)

wheelEventDecoder : (Float -> msg) -> VirtualDom.Handler msg
wheelEventDecoder msg =
  VirtualDom.Custom <|
    Json.Decode.map (\m -> { message = msg m
                           , preventDefault = True
                           , stopPropagation = True
                           }
                    ) <|
    (Json.Decode.field "deltaY" Json.Decode.float)

group : List (Svg msg) -> Svg msg
group things =
  S.g [] things

text1 : Point -> String -> Svg msg
text1 = text_ [ UntypedA.fill (vsColor Foreground) ]

text2 : Point -> String -> Svg msg
text2 =
  text_ [ UntypedA.fill (vsColor Foreground), A.fontWeight T.FontWeightBold ]

text_ : List (Svg.Attribute msg) -> Point -> String -> Svg msg
text_ attrs point str =
  S.text_
    ( [ A.x <| T.px <| G.pointX point
      , A.y <| T.px <| G.pointY point
      , UntypedA.stroke "none" 
      , A.fontFamily [ "Monaco", "Consolas", "Menlo", "Courier New", "monospace" ]
      , A.fontSize (T.px 12)
      ] ++ attrs
    )
    [ Svg.text str
    ]

rect1 : Rect -> Svg msg
rect1 =
  rect_
    [ UntypedA.stroke (vsColor Foreground)
    , UntypedA.fill (vsColor Background)
    ]

rect2 : Rect -> Svg msg
rect2 =
  rect_
    [ UntypedA.stroke (vsColor Red)
    , UntypedA.fill (vsColor Background)
    ]

rect_ : List (Svg.Attribute msg) -> Rect -> Svg msg
rect_ attrs rect =
  S.rect
    ( [ A.x      <| T.px <| G.rectX <| rect
      , A.y      <| T.px <| G.rectY <| rect
      , A.width  <| T.px <| G.rectW <| rect
      , A.height <| T.px <| G.rectH <| rect
      , A.rx     <| T.px 6
      , UntypedA.strokeWidth "2" 
      ] ++ attrs
    )
    []

click msg e =
   S.g
     [ E.onMouseDown msg ]
     [ e ]

arrow1 =
  arrow_ [ UntypedA.stroke (vsColor White) 
         , UntypedA.strokeWidth "2" 
         , UntypedA.markerEnd "url(#arrowHeadFill)"
         ]

arrow2 =
  arrow_ [ UntypedA.strokeDasharray "5,10"
         , UntypedA.stroke (vsColor Red)
         , UntypedA.strokeWidth "2" 
         , UntypedA.markerEnd "url(#arrowHeadLine)" 
         ]

arrow_ : List (Svg.Attribute msg) -> Point -> Point -> Svg msg
arrow_ attrs from to =
  let
    mid = G.midpoint from to
  in S.g
    []
    [ S.line
        ( [ A.x1 <| T.px <| G.pointX mid
          , A.y1 <| T.px <| G.pointY mid
          , A.x2 <| T.px <| G.pointX to
          , A.y2 <| T.px <| G.pointY to
          ] ++ attrs
        )
        []
    , S.line
        ( [ A.x1 <| T.px <| G.pointX from
          , A.y1 <| T.px <| G.pointY from
          , A.x2 <| T.px <| G.pointX mid
          , A.y2 <| T.px <| G.pointY mid
          ] ++ attrs
        )
        []
    ]
