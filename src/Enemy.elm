module Enemy exposing (..)

import AStar exposing (Path)
import Color exposing (Color)
import Coordinates exposing (GameCoordinates)
import Length exposing (Meters)
import Level exposing (Level)
import Point2d exposing (Point2d)
import Point3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Set
import Vector3d


type alias Enemy =
    { position : Point2d Meters GameCoordinates
    , path : Path
    }


init : Level -> Enemy
init level =
    { position = Point2d.meters (toFloat level.start.x - 0.25) (toFloat level.start.y - 0.25)
    , path = level.path
    }


view : Enemy -> Scene3d.Entity GameCoordinates
view enemy =
    Scene3d.quadWithShadow
        (Scene3d.Material.matte Color.red)
        (Point3d.meters 0 0 0)
        (Point3d.meters 0.5 0 0)
        (Point3d.meters 0.5 0.5 0)
        (Point3d.meters 0 0.5 0)
        |> Scene3d.translateBy (Vector3d.xyz (Point2d.xCoordinate enemy.position) (Length.meters 0.2) (Point2d.yCoordinate enemy.position))
