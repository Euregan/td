module Enemy exposing (..)

import AStar exposing (Path)
import Camera exposing (Camera)
import Coordinates exposing (GameCoordinates)
import Direction2d
import Html exposing (Html)
import Length exposing (Meters)
import Level exposing (Level)
import Meshes exposing (Meshes)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity
import Random exposing (Seed)
import Scene3d
import Screen exposing (Screen)
import Ui
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
            Random.step
                (Random.map2 (\x y -> ( x, y ))
                    (Random.float -0.3 0.3)
                    (Random.float -0.4 0.4)
                )
                seed
    in
    ( { position =
            Point2d.meters
                (toFloat level.start.x - 0.25 + offsetX)
                (toFloat level.start.y - 0.25 + offsetY)
      , offset = ( offsetX, offsetY )
      , path = level.path
      , speed = 0.001
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


view : ( Camera, Screen, Level ) -> Meshes -> Enemy -> ( Scene3d.Entity GameCoordinates, Html msg )
view ( camera, screen, level ) meshes enemy =
    let
        position : Float -> Point3d Meters GameCoordinates
        position elevation =
            Point3d.xyz
                (Quantity.plus (Point2d.xCoordinate enemy.position) (Length.meters <| (toFloat level.width / -2)))
                (Length.meters elevation)
                (Quantity.plus (Point2d.yCoordinate enemy.position) (Length.meters <| (toFloat level.length / -2)))

        translation =
            Vector3d.from Point3d.origin (position 0.1)
    in
    ( meshes.characterSkeleton
        |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
        |> Scene3d.group
        |> Scene3d.translateBy translation
        |> Scene3d.scaleAbout (position 0.1) 0.4
    , Ui.healthbar camera screen (position 0.5) ( enemy.currentHp, enemy.maxHp )
    )
