module Geometry exposing (..) {-
  ( Point , point , pointX , pointY
  , move
  , midpoint
  , Rect , rect , rectX , rectY , rectW , rectH
  , corner
  , endCorner
  , center
  , radius
  , lineBetween
  )
-}

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

cornerX : Rect -> Float
cornerX = pointX << corner

cornerY : Rect -> Float
cornerY = pointY << corner

endCorner : Rect -> Point
endCorner (Rect { x2, y2 }) = Point { x = x2, y = y2 }

endCornerX : Rect -> Float
endCornerX = pointX << endCorner

endCornerY : Rect -> Float
endCornerY = pointY << endCorner

center : Rect -> Point
center (Rect { x1, y1, x2, y2 }) =
    Point { x = (x1 + x2) / 2, y = (y1 + y2) / 2 }

centerX : Rect -> Float
centerX = pointX << center

centerY : Rect -> Float
centerY = pointY << center

radius : Rect -> Float
radius r = sqrt <| ((rectW r / 2) ^ 2) + ((rectH r / 2) ^ 2) 

between : Float -> Float -> Float -> Bool
between x alpha beta = x >= alpha && x <= beta

lineBetween : Rect -> Rect -> (Point, Point)
lineBetween from to =
  let
    unsafeM = (centerY to - centerY from) / (centerX to - centerX from)
    m = if isNaN unsafeM || isInfinite unsafeM then 99999999 else unsafeM
    c = (centerY from) - m * (centerX from)
    op = 
        if centerX from < centerX to && centerY from < centerY to then
            { x1 = (+), y1 = (+), x2 = (-), y2 = (-) }
        else if centerX from < centerX to && centerY from > centerY to then
            { x1 = (+), y1 = (-), x2 = (-), y2 = (+) }
        else if centerX from > centerX to && centerY from < centerY to then
            { x1 = (-), y1 = (+), x2 = (+), y2 = (-) }
        else
            { x1 = (-), y1 = (-), x2 = (+), y2 = (+) }
    sX = op.x1 (centerX from) ((rectW from) / 2)
    sY = (m * sX) + c
    (startX, startY) =
        if between sY (cornerY from) (endCornerY from)
        then (sX, sY)
        else let sYAlt = op.y1 (centerY from) ((rectH from) / 2)
                 sXAlt = (sYAlt - c) / m
             in (sXAlt, sYAlt)
    eX = op.x2 (centerX to) ((rectW to) / 2)
    eY = (m * eX) + c
    (endX, endY) =
        if between eY (cornerY to) (endCornerY to)
        then (eX, eY)
        else let eYAlt = op.y2 (centerY to) ((rectH to) / 2)
                 eXAlt = (eYAlt - c) / m
             in (eXAlt, eYAlt)
    in
        (Point { x = startX, y = startY }, Point { x = endX, y = endY })
