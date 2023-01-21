module Camera exposing (..)

import Angle
import Camera3d exposing (Camera3d)
import Coordinates exposing (GameCoordinates)
import Direction3d
import Length exposing (Meters)
import Point3d
import Viewpoint3d


type alias Camera =
    Camera3d Meters GameCoordinates


init : Camera
init =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.meters 40 55 40
                , upDirection = Direction3d.positiveY
                }
        , verticalFieldOfView = Angle.degrees 20
        }
