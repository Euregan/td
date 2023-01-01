module Math exposing (..)

import Coordinates exposing (GameCoordinates)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Quantity
import Rectangle3d exposing (Rectangle3d)
import Triangle3d


pointInRectangle : Rectangle3d Meters GameCoordinates -> Point3d Meters GameCoordinates -> Bool
pointInRectangle rectangle point =
    case Rectangle3d.vertices rectangle of
        [ a, b, c, d ] ->
            let
                rectangleArea =
                    Rectangle3d.area rectangle

                apdArea =
                    Triangle3d.area <| Triangle3d.from a point d

                dpcArea =
                    Triangle3d.area <| Triangle3d.from d point c

                cpbArea =
                    Triangle3d.area <| Triangle3d.from c point b

                pbaArea =
                    Triangle3d.area <| Triangle3d.from point b a
            in
            Quantity.lessThanOrEqualTo
                rectangleArea
                (Quantity.plus
                    (Quantity.plus apdArea dpcArea)
                    (Quantity.plus cpbArea pbaArea)
                )

        _ ->
            False
