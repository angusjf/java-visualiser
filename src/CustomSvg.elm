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

--moveEventDecoder : ((Float, Float) -> msg) -> VirtualDom.Handler msg
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

group things =
  S.g [] things

type VsColor
  = Background
  | Foreground
  | Black
  | Blue
  | BrightBlack
  | BrightBlue
  | BrightCyan
  | BrightGreen
  | BrightMagenta
  | BrightRed
  | BrightWhite
  | BrightYellow
  | Cyan
  | Green
  | Magenta
  | Red
  | White
  | Yellow

vsColor : VsColor -> String
vsColor color =
  case color of 
    Background    ->
        "var(--vscode-terminal-background, var(--vscode-editor-background, black))"
    Foreground    ->
        "var(--vscode-terminal-foreground, var(--vscode-editor-foreground, white))"
    Black         -> "var(--vscode-terminal-ansiBlack, black)"
    Blue          -> "var(--vscode-terminal-ansiBlue, blue)"
    BrightBlack   -> "var(--vscode-terminal-ansiBrightBlack, black)"
    BrightBlue    -> "var(--vscode-terminal-ansiBrightBlue, blue)"
    BrightCyan    -> "var(--vscode-terminal-ansiBrightCyan, cyan)"
    BrightGreen   -> "var(--vscode-terminal-ansiBrightGreen, green)"
    BrightMagenta -> "var(--vscode-terminal-ansiBrightMagenta, magenta)"
    BrightRed     -> "var(--vscode-terminal-ansiBrightRed, red)"
    BrightWhite   -> "var(--vscode-terminal-ansiBrightWhite, white)"
    BrightYellow  -> "var(--vscode-terminal-ansiBrightYellow, yellow)"
    Cyan          -> "var(--vscode-terminal-ansiCyan, cyan)"
    Green         -> "var(--vscode-terminal-ansiGreen, green)"
    Magenta       -> "var(--vscode-terminal-ansiMagenta, magenta)"
    Red           -> "var(--vscode-terminal-ansiRed, red)"
    White         -> "var(--vscode-terminal-ansiWhite, white)"
    Yellow        -> "var(--vscode-terminal-ansiYellow, yellow)"

text1 x y str =
  S.text_
    [ A.x <| T.px x
    , A.y <| T.px y
    , UntypedA.stroke "none" 
    , UntypedA.fill (vsColor Foreground)
    , A.fontFamily [ "Monaco", "Consolas", "Menlo", "Courier New", "monospace" ]
    , A.fontSize (T.px 12)
    ]
    [ Svg.text str
    ]

text2 x y str =
  S.text_
    [ A.x <| T.px x
    , A.y <| T.px y
    , UntypedA.stroke "none" 
    , UntypedA.fill (vsColor Foreground)
    , A.fontWeight T.FontWeightBold
    , A.fontFamily [ "Monaco", "Consolas", "Menlo", "Courier New", "monospace" ]
    , A.fontSize (T.px 12)
    ]
    [ Svg.text str
    ]

rect1 x y w h =
  S.rect
    [ A.x <| T.px x
    , A.y <| T.px y
    , A.width <| T.px w
    , A.height <| T.px h
    , A.rx <| T.px 6
    , UntypedA.stroke (vsColor Foreground)
    , UntypedA.fill (vsColor Background)
    , UntypedA.strokeWidth "2" 
    ]
    []

rect2 x y w h =
  S.rect
    [ A.x <| T.px x
    , A.y <| T.px y
    , A.width <| T.px w
    , A.height <| T.px h
    , A.rx <| T.px 6
    , UntypedA.stroke (vsColor Red)
    , UntypedA.fill (vsColor Background)
    , UntypedA.strokeWidth "2" 
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
  arrow "url(#arrowHeadFill)" [ UntypedA.stroke (vsColor White) 
        , UntypedA.strokeWidth "2" 
        ]

arrow2 =
  arrow "url(#arrowHeadLine)" [ UntypedA.strokeDasharray "5,10"
        , UntypedA.stroke (vsColor Red)
        , UntypedA.strokeWidth "2" 
        ]

arrow head attrs (x1, y1) (x2, y2) =
  let
    xm = (x1 + x2) / 2
    ym = (y1 + y2) / 2
  in S.g
    []
    [ S.line
        ( [ A.x2 <| T.px xm
          , A.y2 <| T.px ym
          , A.x1 <| T.px x2
          , A.y1 <| T.px y2
          ] ++ attrs
        )
        [ ]
    , S.line
        ( [ A.x1 <| T.px x1
          , A.y1 <| T.px y1
          , A.x2 <| T.px xm
          , A.y2 <| T.px ym
          , UntypedA.markerEnd head
          ] ++ attrs
        )
        [ ]
    ]
