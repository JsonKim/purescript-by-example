module Data.Picture where

import Prelude

import Data.Maybe (Maybe(..))

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }

    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

originCircle :: Shape
originCircle = Circle origin 10.0

scale :: Array Shape -> Array Shape
scale = map scale' where
  scale' (Circle _ r) = Circle origin (r * 2.0)
  scale' (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
  scale' s = s

getText :: Shape -> Maybe String
getText (Text _ text) = Just text
getText _ = Nothing
