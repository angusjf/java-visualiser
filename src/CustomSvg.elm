module CustomSvg exposing (..)

import Svg
import TypedSvg as S
import TypedSvg.Attributes as A
import TypedSvg.Events as E
import TypedSvg.Types as T
import Json.Decode
import VirtualDom
import Svg.Attributes as UntypedA

type alias Svg a = Svg.Svg a

render : { width : Float, height : Float, move : (Float, Float) -> msg, up : msg }
      -> List (Svg msg) -> Svg msg
render opts things =
  S.svg
    [ A.width (T.px opts.width)
    , A.height (T.px opts.height)
    , E.on "mousemove" (moveEventDecoder opts.move)
    , E.onMouseUp opts.up
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
          , UntypedA.fill "#fff"
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
          , UntypedA.stroke "#fff"
          , UntypedA.fill "none"
          ] []
        ]
      ]
    , S.g [ ] things
    ]

--moveEventDecoder : ((Float, Float) -> msg) -> VirtualDom.Handler msg
moveEventDecoder msg =
  VirtualDom.Custom <|
    Json.Decode.map (\m -> { message = msg m
                           , preventDefault = True
                           , stopPropagation = True
                           }
                    )<|
      Json.Decode.map2 Tuple.pair
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)

group things =
  S.g [] things

text1 x y str =
  S.text_
    [ A.x <| T.px x
    , A.y <| T.px y
    , UntypedA.stroke "none" 
    , UntypedA.fill "black"
    ]
    [ Svg.text str
    ]

text2 x y str =
  S.text_
    [ A.x <| T.px x
    , A.y <| T.px y
    , UntypedA.stroke "none" 
    , UntypedA.fill "white"
    ]
    [ Svg.text str
    ]

rect1 x y w h =
  S.rect
    [ A.x <| T.px x
    , A.y <| T.px y
    , A.width <| T.px w
    , A.height <| T.px h
    , A.rx <| T.px 15
    , UntypedA.stroke "white" 
    , UntypedA.fill "white"
    , UntypedA.strokeWidth "3" 
    ]
    []

rect2 x y w h =
  S.rect
    [ A.x <| T.px x
    , A.y <| T.px y
    , A.width <| T.px w
    , A.height <| T.px h
    , A.rx <| T.px 15
    , UntypedA.stroke "red" 
    , UntypedA.fill "white"
    , UntypedA.strokeWidth "3" 
    ]
    []

rectClick1 x y w h msg =
   S.g
     [ E.onMouseDown msg
     ]
     [ rect1 x y w h ]

rectClick2 x y w h msg =
   S.g
     [ E.onMouseDown msg
     ]
     [ rect2 x y w h ]

arrow1 =
  arrow [ UntypedA.stroke "white" 
        , UntypedA.strokeWidth "2" 
        , UntypedA.markerEnd "url(#arrowHeadFill)"
        ]

arrow2 =
  arrow [ UntypedA.strokeDasharray "5,10"
        , UntypedA.stroke "#ff4444" 
        , UntypedA.strokeWidth "2" 
        , UntypedA.markerEnd "url(#arrowHeadLine)"
        ]

arrow attrs (x1, y1) (x2, y2) =
  let
    xm = (x1 + x2) / 2
    ym = (y1 + y2) / 2
  in S.g
    []
    [ S.line
        ( [ A.x1 <| T.px xm
          , A.y1 <| T.px ym
          , A.x2 <| T.px x2
          , A.y2 <| T.px y2
          ] ++ attrs
        )
        [ ]
    , S.line
        ( [ A.x1 <| T.px x1
          , A.y1 <| T.px y1
          , A.x2 <| T.px xm
          , A.y2 <| T.px ym
          ] ++ attrs
        )
        [ ]
    ]
