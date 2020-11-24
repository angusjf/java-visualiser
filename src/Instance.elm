module Instance exposing (..)

import Geometry as G exposing (Point, Rect)
import CustomSvg

type alias NodeViewer n msg =
   msg -> n -> Rect -> CustomSvg.Svg msg

type alias EdgeViewer n e msg =
   (n, Rect) -> (n, Rect) -> e -> CustomSvg.Svg msg

type alias RectGetter n = Point -> n -> Rect

type alias ClickHandler n = n -> n

type alias Instance n e msg =
  { viewNode : NodeViewer n msg
  , viewEdge : EdgeViewer n e msg
  , getRect : RectGetter n
  , onClick : ClickHandler n
  }
