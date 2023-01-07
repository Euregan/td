module Enemy exposing (..)

import AStar exposing (Path, Position)
import Color
import Coordinates exposing (GameCoordinates)
import Direction2d
import Length exposing (Meters)
import Level exposing (Level)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Scene3d
import Scene3d.Material
import Vector2d
import Vector3d


type alias Enemy =
    { position : Point2d Meters GameCoordinates
    , path : Path
    , speed : Float
    }


init : Level -> Enemy
init level =
    { position = Point2d.meters (toFloat level.start.x - 0.25) (toFloat level.start.y - 0.25)
    , path = level.path
    , speed = 0.002
    }


tick : Float -> Enemy -> Enemy
tick delta enemy =
    let
        destination =
            List.head enemy.path
                |> Maybe.map (\( x, y ) -> Point2d.meters (toFloat x) (toFloat y))

        maybeDirection =
            Maybe.andThen (\dest -> Maybe.map (\direction -> ( direction, dest )) (Direction2d.from enemy.position dest)) destination
    in
    case maybeDirection of
        Nothing ->
            case enemy.path of
                _ :: path ->
                    tick delta { enemy | path = path }

                [] ->
                    enemy

        Just ( direction, dest ) ->
            let
                target =
                    Point2d.translateIn direction (Length.meters (enemy.speed * delta)) enemy.position

                distance =
                    Point2d.distanceFrom enemy.position dest
            in
            if Quantity.lessThan (Length.meters <| enemy.speed * delta) distance then
                tick delta { enemy | path = List.drop 1 enemy.path }

            else
                { enemy
                    | position = target
                }


view : Enemy -> Scene3d.Entity GameCoordinates
view enemy =
    Scene3d.quadWithShadow
        (Scene3d.Material.matte Color.red)
        (Point3d.meters -0.25 0 0)
        (Point3d.meters 0.25 0 0)
        (Point3d.meters 0.25 0.5 0)
        (Point3d.meters -0.25 0.5 0)
        |> Scene3d.translateBy (Vector3d.xyz (Point2d.xCoordinate enemy.position) (Length.meters 0.2) (Point2d.yCoordinate enemy.position))
