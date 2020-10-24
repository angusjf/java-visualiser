module Graphics exposing (..)

import Svg as S
import Svg.Attributes as A

type alias Svg msg = S.Svg msg

render things =
  S.svg
    [ A.width "600"
    , A.height "600"
    ]
    things

group things = S.g [] things

text x y str = S.text_
  [ A.x <| String.fromInt x
  , A.y <| String.fromInt y
  , A.stroke "none" 
  , A.fill "white"
  ]
  [ S.text str
  ]

rect1 x y w h = S.rect
  [ A.x <| String.fromInt x
  , A.y <| String.fromInt y
  , A.width <| String.fromInt w
  , A.height <| String.fromInt h
  , A.rx <| String.fromInt 15
  , A.stroke "white" 
  , A.fill "none"
  ]
  []

rect2 x y w h = S.rect
  [ A.x <| String.fromInt x
  , A.y <| String.fromInt y
  , A.width <| String.fromInt w
  , A.height <| String.fromInt h
  , A.rx <| String.fromInt 15
  , A.stroke "red" 
  , A.fill "none"
  ]
  []
