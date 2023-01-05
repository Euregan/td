module GameState exposing (..)

import AStar exposing (Position)
import Axis3d exposing (Axis3d)
import Camera exposing (Camera)
import Camera3d
import Coordinates exposing (GameCoordinates)
import Enemy exposing (Enemy)
import Length exposing (Meters)
import Level exposing (Level)
import Math exposing (pointInRectangle)
import Meshes exposing (Meshes)
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity)
import Rectangle2d
import Rectangle3d
import Scene3d
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)
import Viewport exposing (Viewport)


type alias MousePosition =
    { x : Quantity Float Pixels, y : Quantity Float Pixels }


type alias GameState =
    { level : Level
    , meshes : Meshes
    , viewport : Viewport
    , camera : Camera
    , hoveredTile : Maybe Position
    , enemies : List Enemy
    }


type Msg
    = WindowResized Viewport
    | MouseMoved (Quantity Float Pixels) (Quantity Float Pixels)


init : Viewport -> Meshes -> GameState
init viewport meshes =
    let
        level =
            Level.init ( 15, 25 )
    in
    { level = level
    , viewport = viewport
    , meshes = meshes
    , camera = Camera.init
    , hoveredTile = Nothing
    , enemies = [ Enemy.init level ]
    }


update : Msg -> GameState -> GameState
update msg state =
    case msg of
        WindowResized viewport ->
            { state | viewport = viewport }

        MouseMoved x y ->
            let
                axis =
                    Camera3d.ray
                        state.camera
                        (Rectangle2d.from
                            (Point2d.pixels 0 state.viewport.height)
                            (Point2d.pixels state.viewport.width 0)
                        )
                        (Point2d.xy x y)

                hoveredTile =
                    Level.tiles state.level
                        |> List.filter (intersectWithTile axis state.level)
                        |> List.head
            in
            { state | hoveredTile = hoveredTile }


view : GameState -> List (Scene3d.Entity GameCoordinates)
view state =
    List.map Enemy.view state.enemies
        |> List.map
            (Scene3d.translateBy
                (Vector3d.meters
                    (toFloat state.level.width / -2)
                    0
                    (toFloat state.level.length / -2)
                )
            )


intersectWithTile : Axis3d Meters GameCoordinates -> Level -> Position -> Bool
intersectWithTile axis level ( x, y ) =
    Axis3d.intersectionWithPlane (SketchPlane3d.toPlane plane) axis
        |> Maybe.map
            (Rectangle2d.from
                (Point2d.meters (toFloat x + 0.5 - toFloat level.width / 2) (toFloat y + 0.5 - toFloat level.length / 2))
                (Point2d.meters (toFloat x - 0.5 - toFloat level.width / 2) (toFloat y - 0.5 - toFloat level.length / 2))
                |> Rectangle3d.on plane
                |> pointInRectangle
            )
        |> Maybe.withDefault False


plane : SketchPlane3d Meters GameCoordinates defines
plane =
    SketchPlane3d.xz
