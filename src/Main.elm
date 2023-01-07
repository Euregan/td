module Main exposing (..)

import Angle
import Browser
import Browser.Events
import Color
import Coordinates exposing (GameCoordinates)
import Direction3d
import Flags exposing (Flags)
import GameState exposing (GameState)
import Html exposing (Html, text)
import Json.Decode
import Length
import Level
import LineSegment3d
import Loading
import Msg exposing (Msg(..))
import Pixels
import Point3d
import Scene3d
import Scene3d.Material
import Viewport exposing (Viewport)


type alias LoadingMeshes =
    { viewport : Viewport
    , meshes : Loading.Loading
    }


type Model
    = Loading LoadingMeshes
    | Loaded GameState
    | Error


main : Program Flags Model (Msg Loading.Msg)
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd (Msg Loading.Msg) )
init { viewport, models } =
    ( Loading
        { viewport = viewport
        , meshes = Loading.init
        }
    , Cmd.batch <|
        Loading.commands models
    )


update : Msg Loading.Msg -> Model -> ( Model, Cmd (Msg Loading.Msg) )
update msg model =
    case model of
        Loading meshes ->
            let
                handleState : LoadingMeshes -> Model
                handleState state =
                    case Loading.toMeshes state.meshes of
                        Nothing ->
                            Loading state

                        Just (Err _) ->
                            Error

                        Just (Ok models) ->
                            Loaded <| GameState.init state.viewport models
            in
            case msg of
                GotMesh toMsg result ->
                    ( handleState { meshes | meshes = Loading.update meshes.meshes (toMsg result) }, Cmd.none )

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

                NewFrame delta ->
                    ( Loaded <| GameState.tick delta state, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Error ->
            ( model, Cmd.none )


view : Model -> Html (Msg Loading.Msg)
view model =
    case model of
        Loading _ ->
            text "🤔"

        Loaded state ->
            let
                { level, meshes, viewport, camera, hoveredTile } =
                    state
            in
            Scene3d.sunny
                { dimensions = ( Pixels.int <| floor viewport.width, Pixels.int <| floor viewport.height )
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , entities = List.concat [ [ ground ], Level.view meshes level hoveredTile, GameState.view meshes state ]
                , shadows = True
                , sunlightDirection = Direction3d.xyZ (Angle.degrees 130) (Angle.degrees 150)
                , upDirection = Direction3d.positiveY
                }

        Error ->
            text "😔"


subscriptions : Model -> Sub (Msg Loading.Msg)
subscriptions model =
    let
        decodeMouseMove : Json.Decode.Decoder (Msg Loading.Msg)
        decodeMouseMove =
            Json.Decode.map2 MouseMoved
                (Json.Decode.field "pageX" (Json.Decode.map Pixels.float Json.Decode.float))
                (Json.Decode.field "pageY" (Json.Decode.map Pixels.float Json.Decode.float))
    in
    Sub.batch <|
        List.concat
            [ [ Browser.Events.onResize (\width height -> WindowResized <| Viewport (toFloat width) (toFloat height))
              , Browser.Events.onMouseMove decodeMouseMove
              ]
            , case model of
                Loaded _ ->
                    [ Browser.Events.onAnimationFrameDelta NewFrame ]

                _ ->
                    []
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
