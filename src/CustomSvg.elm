module CustomSvg exposing (..)

import Svg as S
import Svg.Attributes as A
import Svg.Events as E
import Json.Decode

type alias Svg msg = S.Svg msg

render opts things =
  S.svg
    [ A.width "600"
    , A.height "800"
    , E.on "mousemove" opts.move
    , E.on "mouseup" opts.up
    ]
    [ S.g [ ] things ]

group things =
  S.g [] things

text x y str =
  S.text_
    [ A.x <| String.fromFloat x
    , A.y <| String.fromFloat y
    , A.stroke "none" 
    , A.fill "black"
    ]
    [ S.text str
    ]

rect1 x y w h =
  S.rect
    [ A.x <| String.fromFloat x
    , A.y <| String.fromFloat y
    , A.width <| String.fromFloat w
    , A.height <| String.fromFloat h
    , A.rx <| String.fromFloat 15
    , A.stroke "white" 
    , A.fill "white"
    , A.strokeWidth "3" 
    ]
    []

rect2 x y w h =
  S.rect
    [ A.x <| String.fromFloat x
    , A.y <| String.fromFloat y
    , A.width <| String.fromFloat w
    , A.height <| String.fromFloat h
    , A.rx <| String.fromFloat 15
    , A.stroke "red" 
    , A.fill "red"
    , A.strokeWidth "3" 
    ]
    []

rectClick1 x y w h decoder =
   S.g
     [ E.on "mousedown" decoder
     ]
     [ rect1 x y w h ]

rectClick2 x y w h decoder =
   S.g
     [ E.on "mousedown" decoder
     ]
     [ rect2 x y w h ]

arrow1 (x1, y1) (x2, y2) =
  S.g
    []
    [ S.line
      [ A.x1 <| String.fromFloat x1
      , A.y1 <| String.fromFloat y1
      , A.x2 <| String.fromFloat x2
      , A.y2 <| String.fromFloat y2
      , A.stroke "white" 
      , A.strokeWidth "2" 
      ]
      []
    , text x2 (y2 - 10) "^"
    ]

on2 str decoder =
  E.stopPropagationOn str (Json.Decode.map (\x -> (x, True)) decoder)
