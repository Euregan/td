module GameState exposing (..)

import AStar exposing (Position)
import Axis3d exposing (Axis3d)
import Building exposing (Building)
import Camera exposing (Camera)
import Camera3d
import Coordinates exposing (GameCoordinates)
import Enemy exposing (Enemy)
import Length exposing (Meters)
import Level exposing (Level)
import Math exposing (pointInRectangle)
import Meshes exposing (Meshes)
import Pixels exposing (Pixels)
import Player exposing (Player)
import Point2d
import Quantity exposing (Quantity)
import Random exposing (Seed)
import Rectangle2d
import Rectangle3d
import Scene3d
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d
import Viewport exposing (Viewport)
import Wave exposing (Wave)


type alias MousePosition =
    { x : Quantity Float Pixels, y : Quantity Float Pixels }


type alias GameState =
    { level : Level
    , meshes : Meshes
    , viewport : Viewport
    , camera : Camera
    , hoveredTile : Maybe Position
    , players : List Player
    , player : Player
    , wave : Wave
    , mouseDown : Maybe MousePosition
    }


type Msg
    = WindowResized Viewport
    | MouseMoved (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseDown (Quantity Float Pixels) (Quantity Float Pixels)


init : Seed -> Viewport -> Meshes -> ( GameState, Seed )
init seed viewport meshes =
    let
        level =
            Level.init ( 15, 25 )

        ( wave, finalSeed ) =
            Wave.init seed level
    in
    ( { level = level
      , viewport = viewport
      , meshes = meshes
      , camera = Camera.init
      , hoveredTile = Nothing
      , players = []
      , player = Player.init
      , wave = wave
      , mouseDown = Nothing
      }
    , finalSeed
    )


update : Msg -> GameState -> GameState
update msg state =
    case msg of
        WindowResized viewport ->
            { state | viewport = viewport }

        MouseMoved x y ->
            { state | hoveredTile = pointedTile state.camera state.viewport state.level { x = x, y = y } }

        MouseDown x y ->
            let
                tile =
                    pointedTile state.camera state.viewport state.level { x = x, y = y }

                player =
                    case tile of
                        Nothing ->
                            state.player

                        Just position ->
                            Player.build state.player position

                ( level, wave ) =
                    case tile of
                        Nothing ->
                            ( state.level, state.wave )

                        Just position ->
                            let
                                buildings =
                                    position
                                        :: (List.map (\building -> building.position) <|
                                                List.concat [ state.player.buildings, List.concat <| List.map (\p -> p.buildings) <| state.players ]
                                           )

                                updatedLevel =
                                    Level.update state.level <| Level.BuildingsChanged buildings
                            in
                            ( updatedLevel
                            , { enemies = List.map (\enemy -> Enemy.update enemy <| Enemy.BuildingsChanged updatedLevel buildings) state.wave.enemies }
                            )
            in
            { state | mouseDown = Just { x = x, y = y }, player = player, level = level, wave = wave }

        MouseUp _ _ ->
            { state | mouseDown = Nothing }


tick : Float -> GameState -> GameState
tick delta state =
    let
        updatedWave =
            Wave.tick delta state.wave

        ( player, enemies ) =
            Player.tick delta updatedWave.enemies state.player

        finalWave =
            { updatedWave | enemies = enemies }
    in
    { state
        | wave = finalWave
        , player = player
    }


view : Meshes -> GameState -> List (Scene3d.Entity GameCoordinates)
view meshes state =
    List.concat
        [ Wave.view state.wave state.level
        , Player.view meshes state.player state.hoveredTile
            |> List.map
                (Scene3d.translateBy
                    (Vector3d.meters
                        (toFloat state.level.width / -2)
                        0
                        (toFloat state.level.length / -2)
                    )
                )
        ]


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


pointedTile : Camera3d.Camera3d Meters GameCoordinates -> Viewport -> Level -> MousePosition -> Maybe Position
pointedTile camera viewport level { x, y } =
    let
        axis =
            Camera3d.ray
                camera
                (Rectangle2d.from
                    (Point2d.pixels 0 viewport.height)
                    (Point2d.pixels viewport.width 0)
                )
                (Point2d.xy x y)
    in
    Level.tiles level
        |> List.filter (intersectWithTile axis level)
        |> List.head
