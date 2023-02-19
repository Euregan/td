module Camera exposing (Camera, init, zoom)

import Angle
import Axis3d
import Camera3d exposing (Camera3d)
import Coordinates exposing (GameCoordinates)
import Direction3d
import Length exposing (Meters)
import Plane3d
import Point3d
import Quantity
import Viewpoint3d exposing (eyePoint)


type alias Camera =
    Camera3d Meters GameCoordinates


fov : Angle.Angle
fov =
    Angle.degrees 20


init : Camera
init =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.meters 40 55 40
                , upDirection = Direction3d.positiveY
                }
        , verticalFieldOfView = fov
        }


zoom : Float -> Camera -> Camera
zoom level camera =
    let
        viewpoint =
            Camera3d.viewpoint camera

        eyePoint =
            Viewpoint3d.eyePoint viewpoint

        viewDirection =
            Viewpoint3d.viewDirection viewpoint

        viewAxis =
            Axis3d.withDirection viewDirection eyePoint

        newEyePoint =
            Point3d.along
                viewAxis
                (Length.meters (level / 25))

        floor : Plane3d.Plane3d Meters GameCoordinates
        floor =
            Plane3d.through (Point3d.meters 0 0 8) Direction3d.z

        ceiling : Plane3d.Plane3d Meters GameCoordinates
        ceiling =
            Plane3d.through (Point3d.meters 0 0 50) Direction3d.z

        maybeMaxZoomedInEyepoint =
            Axis3d.intersectionWithPlane floor viewAxis

        maybeMaxZoomedOutEyepoint =
            Axis3d.intersectionWithPlane ceiling viewAxis

        restrictedEyePoint =
            Maybe.map2
                (\maxZoomedInEyepoint maxZoomedOutEyepoint ->
                    if Quantity.lessThan (Length.meters 0) (Point3d.signedDistanceAlong (Axis3d.withDirection viewDirection maxZoomedInEyepoint |> Axis3d.reverse) newEyePoint) then
                        maxZoomedInEyepoint

                    else if Quantity.greaterThan (Length.meters 0) (Point3d.signedDistanceAlong (Axis3d.withDirection viewDirection maxZoomedOutEyepoint |> Axis3d.reverse) newEyePoint) then
                        maxZoomedOutEyepoint

                    else
                        newEyePoint
                )
                maybeMaxZoomedInEyepoint
                maybeMaxZoomedOutEyepoint
                |> Maybe.withDefault newEyePoint
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = restrictedEyePoint
                , upDirection = Direction3d.positiveY
                }
        , verticalFieldOfView = fov
        }
