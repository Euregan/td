module Enemy exposing (..)

import AStar exposing (Path)
import Color
import Coordinates exposing (GameCoordinates)
import Direction2d
import Length exposing (Meters)
import Level exposing (Level)
import Point2d exposing (Point2d)
import Point3d
import Quantity
import Random exposing (Seed)
import Scene3d
import Scene3d.Material
import Vector3d


type alias Enemy =
    { position : Point2d Meters GameCoordinates
    , offset : ( Float, Float )
    , path : Path
    , speed : Float
    , currentHp : Int
    , maxHp : Int
    }


type Msg
    = BuildingsChanged Level (List ( Int, Int ))


init : Seed -> Level -> ( Enemy, Seed )
init seed level =
    let
        ( ( offsetX, offsetY ), newSeed ) =
            Random.step (Random.map2 (\x y -> ( x, y )) (Random.float -0.3 0.3) (Random.float -0.4 0.4)) seed
    in
    ( { position = Point2d.meters (toFloat level.start.x - 0.25 + offsetX) (toFloat level.start.y - 0.25 + offsetY)
      , offset = ( offsetX, offsetY )
      , path = level.path
      , speed = 0.002
      , maxHp = 20
      , currentHp = 20
      }
    , newSeed
    )


update : Enemy -> Msg -> Enemy
update enemy msg =
    case msg of
        BuildingsChanged level buildings ->
            let
                newPath =
                    case List.head enemy.path of
                        Nothing ->
                            []

                        Just ( x, y ) ->
                            Level.path level.width level.length { x = x, y = y } level.end buildings
            in
            { enemy | path = newPath }


tick : Float -> Enemy -> Enemy
tick delta enemy =
    let
        destination =
            List.head enemy.path
                |> Maybe.map (\( x, y ) -> Point2d.meters (toFloat x + Tuple.first enemy.offset) (toFloat y))

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
