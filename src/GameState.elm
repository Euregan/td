module GameState exposing (..)

import AStar exposing (Position)
import Axis3d exposing (Axis3d)
import Camera exposing (Camera)
import Camera3d
import Coordinates exposing (GameCoordinates)
import Length exposing (Meters)
import Level exposing (Level)
import Math exposing (pointInRectangle)
import Meshes exposing (Meshes)
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Quantity exposing (Quantity)
import Rectangle2d
import Rectangle3d
import SketchPlane3d exposing (SketchPlane3d)
import Viewport exposing (Viewport)


type alias MousePosition =
    { x : Quantity Float Pixels, y : Quantity Float Pixels }


type alias GameState =
    { level : Level
    , meshes : Meshes
    , viewport : Viewport
    , camera : Camera
    , hoveredTile : Maybe Position
    }


type Msg
    = WindowResized Viewport
    | MouseMoved (Quantity Float Pixels) (Quantity Float Pixels)


init : Viewport -> Meshes -> GameState
init viewport meshes =
    { level = Level.init ( 15, 25 )
    , viewport = viewport
    , meshes = meshes
    , camera = Camera.init
    , hoveredTile = Nothing
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
                        (Rectangle2d.with
                            { x1 = Pixels.float 0
                            , y1 = Pixels.float 0
                            , x2 = Pixels.float state.viewport.width
                            , y2 = Pixels.float state.viewport.height
                            }
                        )
                        (Point2d.xy x y)

                hoveredTile =
                    Level.tiles state.level
                        |> List.filter (intersectWithTile axis state.level)
                        |> List.head
            in
            { state | hoveredTile = hoveredTile }


intersectWithTile : Axis3d Meters GameCoordinates -> Level -> Position -> Bool
intersectWithTile axis level ( x, y ) =
    Axis3d.intersectionWithPlane Plane3d.zx axis
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
    SketchPlane3d.zx |> SketchPlane3d.reverseY |> SketchPlane3d.reverseX
