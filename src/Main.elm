module Main exposing (..)

import Angle
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Coordinates exposing (GameCoordinates)
import Direction3d
import GBL.Decode
import GameState exposing (GameState)
import Html exposing (Html, text)
import Http
import Json.Decode
import Length
import Level
import LineSegment3d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import SketchPlane3d exposing (SketchPlane3d)
import Viewport exposing (Viewport)


getMesh : String -> (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )) -> Msg) -> Cmd Msg
getMesh model msg =
    Http.get
        { url = model
        , expect =
            GBL.Decode.expectGBL msg
        }


type alias LoadingMeshes =
    { viewport : Viewport
    , tile : Maybe (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    , spawn : Maybe (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    , straight : Maybe (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    , corner : Maybe (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    , end : Maybe (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    }


type Model
    = Loading LoadingMeshes
    | Loaded GameState
    | Error


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    { viewport : Viewport
    , models :
        { tile : String
        , spawn : String
        , straight : String
        , corner : String
        , end : String
        }
    }


init : Flags -> ( Model, Cmd Msg )
init { viewport, models } =
    ( Loading
        { viewport = viewport
        , tile = Nothing
        , spawn = Nothing
        , straight = Nothing
        , corner = Nothing
        , end = Nothing
        }
    , Cmd.batch
        [ getMesh models.tile GotTile
        , getMesh models.spawn GotSpawn
        , getMesh models.straight GotStraight
        , getMesh models.corner GotCorner
        , getMesh models.end GotEnd
        ]
    )


type Msg
    = GotTile (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    | GotSpawn (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    | GotStraight (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    | GotCorner (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    | GotEnd (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )))
    | WindowResized Viewport
    | MouseMoved (Quantity Float Pixels) (Quantity Float Pixels)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading meshes ->
            let
                checkState : LoadingMeshes -> Model
                checkState { viewport, tile, spawn, straight, corner, end } =
                    case [ tile, spawn, straight, corner, end ] of
                        [ Just (Ok loadedTile), Just (Ok loadedSpawn), Just (Ok loadedStraight), Just (Ok loadedCorner), Just (Ok loadedEnd) ] ->
                            Loaded <| GameState.init viewport { tile = loadedTile, spawn = loadedSpawn, straight = loadedStraight, corner = loadedCorner, end = loadedEnd }

                        [ Nothing, _, _, _, _ ] ->
                            Loading { viewport = viewport, tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, Nothing, _, _, _ ] ->
                            Loading { viewport = viewport, tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, Nothing, _, _ ] ->
                            Loading { viewport = viewport, tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, _, Nothing, _ ] ->
                            Loading { viewport = viewport, tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, _, _, Nothing ] ->
                            Loading { viewport = viewport, tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        _ ->
                            Error
            in
            case msg of
                GotTile mesh ->
                    ( checkState { meshes | tile = Just mesh }, Cmd.none )

                GotSpawn mesh ->
                    ( checkState { meshes | spawn = Just mesh }, Cmd.none )

                GotStraight mesh ->
                    ( checkState { meshes | straight = Just mesh }, Cmd.none )

                GotCorner mesh ->
                    ( checkState { meshes | corner = Just mesh }, Cmd.none )

                GotEnd mesh ->
                    ( checkState { meshes | end = Just mesh }, Cmd.none )

                WindowResized viewport ->
                    ( Loading { meshes | viewport = viewport }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Loaded state ->
            case msg of
                WindowResized viewport ->
                    ( Loaded <| GameState.update (GameState.WindowResized viewport) state, Cmd.none )

                MouseMoved x y ->
                    ( Loaded <| GameState.update (GameState.MouseMoved x y) state, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Error ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "\u{1F914}"

        Loaded { level, meshes, viewport, camera, hoveredTile } ->
            Scene3d.sunny
                { dimensions = ( Pixels.int <| floor viewport.width, Pixels.int <| floor viewport.height )
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , entities =
                    ground :: Level.view meshes level hoveredTile
                , shadows = True
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 130) (Angle.degrees 150)
                , upDirection = Direction3d.positiveY
                }

        Error ->
            text "😔"


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        decodeMouseMove : Json.Decode.Decoder Msg
        decodeMouseMove =
            Json.Decode.map2 MouseMoved
                (Json.Decode.field "pageX" (Json.Decode.map Pixels.float Json.Decode.float))
                (Json.Decode.field "pageY" (Json.Decode.map Pixels.float Json.Decode.float))
    in
    Sub.batch
        [ Browser.Events.onResize (\width height -> WindowResized <| Viewport (toFloat width) (toFloat height))
        , Browser.Events.onMouseMove decodeMouseMove
        ]


ground : Scene3d.Entity GameCoordinates
ground =
    Scene3d.quad
        (Scene3d.Material.matte Color.white)
        (Point3d.meters -50 0 -50)
        (Point3d.meters -50 0 50)
        (Point3d.meters 50 0 50)
        (Point3d.meters 50 0 -50)


grid : Scene3d.Entity GameCoordinates
grid =
    [ List.range -11 10
        |> List.map
            (\x ->
                Scene3d.lineSegment
                    (Scene3d.Material.color Color.red)
                    (LineSegment3d.from (Point3d.meters (toFloat x) 0 -16) (Point3d.meters (toFloat x) 0 15))
            )
    , List.range -16 15
        |> List.map
            (\y ->
                Scene3d.lineSegment
                    (Scene3d.Material.color Color.red)
                    (LineSegment3d.from (Point3d.meters -11 0 (toFloat y)) (Point3d.meters 10 0 (toFloat y)))
            )
    ]
        |> List.concat
        |> Scene3d.group
