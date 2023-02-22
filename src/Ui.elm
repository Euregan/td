module Ui exposing (..)

import Camera exposing (Camera)
import Coordinates exposing (GameCoordinates)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Length exposing (Meters)
import Pixels
import Point2d
import Point3d exposing (Point3d)
import Point3d.Projection
import Screen exposing (Screen)


healthbar : Camera -> Screen -> Point3d Meters GameCoordinates -> ( Int, Int ) -> Html msg
healthbar camera screen inGamePosition ( current, max ) =
    let
        position =
            Point3d.Projection.toScreenSpace camera screen inGamePosition
    in
    div
        [ style "position" "absolute"
        , style "left" <| (String.fromFloat <| Pixels.inPixels <| Point2d.xCoordinate position) ++ "px"
        , style "top" <| (String.fromFloat <| Pixels.inPixels <| Point2d.yCoordinate position) ++ "px"
        , style "transform" "translateX(-50%)"
        ]
        [ text <| String.fromInt current ++ "/" ++ String.fromInt max ]
