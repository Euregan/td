module Screen exposing (Screen, ScreenCoordinates, init)

import Pixels exposing (Pixels)
import Point2d
import Rectangle2d exposing (Rectangle2d)
import Viewport exposing (Viewport)


type ScreenCoordinates
    = ScreenCoordinates


type alias Screen =
    Rectangle2d Pixels ScreenCoordinates


init : Viewport -> Screen
init { width, height } =
    Rectangle2d.from
        (Point2d.pixels 0 height)
        (Point2d.pixels width 0)
