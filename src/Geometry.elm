module Geometry exposing
  ( Point
  , point
  , pointX
  , pointY
  , move
  , midpoint
  , Rect
  , rect
  , rectX
  , rectY
  , rectW
  , rectH
  , corner
  , middle
  , radius
  )

type Point = Point { x : Float, y : Float }

type Rect = Rect { x1 : Float, y1 : Float, x2 : Float, y2 : Float }

point : Float -> Float -> Point
point x y = Point { x = x, y = y }

pointX : Point -> Float
pointX (Point { x }) = x

pointY : Point -> Float
pointY (Point { y }) = y

move : Point -> Float -> Float -> Point
move (Point { x, y }) dx dy = Point { x = x + dx, y = y + dy }

midpoint : Point -> Point -> Point
midpoint (Point a) (Point b) =
    Point { x = (a.x + b.x) / 2, y = (a.y + b.y) / 2 }

rect : Point -> Float -> Float -> Rect
rect (Point { x, y }) width height =
  Rect
    { x1 = x - width  / 2
    , y1 = y - height / 2
    , x2 = x + width  / 2
    , y2 = y + height / 2
    }

rectX : Rect -> Float
rectX (Rect { x1 }) = x1

rectY : Rect -> Float
rectY (Rect { y1 }) = y1

rectW : Rect -> Float
rectW (Rect { x1, x2 }) = abs (x1 - x2)

rectH : Rect -> Float
rectH (Rect { y1, y2 }) = abs (y1 - y2)

corner : Rect -> Point
corner (Rect { x1, y1 }) = Point { x = x1, y = y1 }

middle : Rect -> Point
middle (Rect { x1, y1, x2, y2 }) =
    Point { x = (x1 + x2) / 2, y = (y1 + y2) / 2 }

radius : Rect -> Float
radius r = sqrt <| ((rectW r / 2) ^ 2) + ((rectH r / 2) ^ 2) 
    
