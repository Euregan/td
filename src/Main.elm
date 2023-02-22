port module Main exposing (..)

import Angle
import Browser
import Browser.Events
import Color
import Coordinates exposing (GameCoordinates)
import Direction3d
import Flags exposing (Flags)
import GameState exposing (GameState)
import Html exposing (Html, main_, text)
import Illuminance
import Json.Decode
import Length
import Level
import LineSegment3d
import Loading
import Msg exposing (Msg(..))
import Pixels
import Point3d
import Quantity
import Random exposing (Seed)
import Rectangle2d
import Scene3d
import Scene3d.Light
import Scene3d.Material
import Screen exposing (Screen)
import Viewport exposing (Viewport)


type alias LoadingMeshes =
    { screen : Screen
    , meshes : Loading.Loading
    }


type Model
    = Loading Seed LoadingMeshes
    | Loaded Seed GameState
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
init { viewport, models, seed } =
    ( Loading
        (Random.initialSeed seed)
        { screen = Screen.init viewport
        , meshes = Loading.init
        }
    , Cmd.batch <|
        Loading.commands models
    )


update : Msg Loading.Msg -> Model -> ( Model, Cmd (Msg Loading.Msg) )
update msg model =
    case model of
        Loading seed meshes ->
            let
                handleState : LoadingMeshes -> Model
                handleState loading =
                    case Loading.toMeshes loading.meshes of
                        Nothing ->
                            Loading seed loading

                        Just (Err _) ->
                            Error

                        Just (Ok models) ->
                            let
                                ( state, finalSeed ) =
                                    GameState.init seed loading.screen models
                            in
                            Loaded finalSeed state
            in
            case msg of
                GotMesh toMsg result ->
                    ( handleState { meshes | meshes = Loading.update meshes.meshes (toMsg result) }, Cmd.none )

                WindowResized viewport ->
                    ( Loading seed { meshes | screen = Screen.init viewport }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Loaded seed state ->
            case msg of
                WindowResized viewport ->
                    ( Loaded seed <| GameState.update (GameState.WindowResized viewport) state, Cmd.none )

                MouseMoved x y ->
                    ( Loaded seed <| GameState.update (GameState.MouseMoved x y) state, Cmd.none )

                MouseDown x y ->
                    ( Loaded seed <| GameState.update (GameState.MouseDown x y) state, Cmd.none )

                MouseUp x y ->
                    ( Loaded seed <| GameState.update (GameState.MouseUp x y) state, Cmd.none )

                NewFrame delta ->
                    ( Loaded seed <| GameState.tick delta state, Cmd.none )

                OnWheel delta ->
                    ( Loaded seed <| GameState.update (GameState.OnWheel delta) state, Cmd.none )

                GotMesh _ _ ->
                    ( model, Cmd.none )

        Error ->
            ( model, Cmd.none )


view : Model -> Html (Msg Loading.Msg)
view model =
    case model of
        Loading _ _ ->
            text "🤔"

        Loaded _ state ->
            let
                { level, meshes, screen, camera, hoveredTile } =
                    state

                ( gameEntities, gameUi ) =
                    GameState.view meshes state
                        |> List.foldl (\( entity, ui ) ( entities, uis ) -> ( entity :: entities, ui :: uis )) ( [], [] )
            in
            main_ []
                (Scene3d.custom
                    { lights =
                        Scene3d.twoLights
                            (Scene3d.Light.directional (Scene3d.Light.castsShadows True)
                                { chromaticity = Scene3d.Light.sunlight
                                , intensity = Illuminance.lux 80000
                                , direction = Direction3d.xyZ (Angle.degrees 130) (Angle.degrees 150)
                                }
                            )
                            (Scene3d.Light.ambient
                                { chromaticity = Scene3d.Light.sunlight
                                , intensity = Illuminance.lux 13000
                                }
                            )
                    , camera = camera
                    , clipDepth = Length.meters 1
                    , exposure = Scene3d.exposureValue 13
                    , toneMapping = Scene3d.noToneMapping
                    , whiteBalance = Scene3d.Light.daylight
                    , antialiasing = Scene3d.multisampling
                    , dimensions = Rectangle2d.dimensions screen |> Tuple.mapBoth Quantity.floor Quantity.floor
                    , background = Scene3d.transparentBackground
                    , entities = List.concat [ [ ground ], Level.view meshes level hoveredTile, gameEntities ]
                    }
                    :: gameUi
                )

        Error ->
            text "😔"


port onWheel : (Float -> msg) -> Sub msg


subscriptions : Model -> Sub (Msg Loading.Msg)
subscriptions model =
    let
        decodeMouseMove : Json.Decode.Decoder (Msg Loading.Msg)
        decodeMouseMove =
            Json.Decode.map2 MouseMoved
                (Json.Decode.field "pageX" (Json.Decode.map Pixels.float Json.Decode.float))
                (Json.Decode.field "pageY" (Json.Decode.map Pixels.float Json.Decode.float))

        decodeMouseDown : Json.Decode.Decoder (Msg Loading.Msg)
        decodeMouseDown =
            Json.Decode.map2 MouseDown
                (Json.Decode.field "pageX" (Json.Decode.map Pixels.float Json.Decode.float))
                (Json.Decode.field "pageY" (Json.Decode.map Pixels.float Json.Decode.float))

        decodeMouseUp : Json.Decode.Decoder (Msg Loading.Msg)
        decodeMouseUp =
            Json.Decode.map2 MouseUp
                (Json.Decode.field "pageX" (Json.Decode.map Pixels.float Json.Decode.float))
                (Json.Decode.field "pageY" (Json.Decode.map Pixels.float Json.Decode.float))
    in
    Sub.batch <|
        List.concat
            [ [ Browser.Events.onResize (\width height -> WindowResized <| Viewport (toFloat width) (toFloat height))
              ]
            , case model of
                Loaded _ _ ->
                    [ Browser.Events.onAnimationFrameDelta NewFrame
                    , Browser.Events.onMouseMove decodeMouseMove
                    , Browser.Events.onMouseDown decodeMouseDown
                    , Browser.Events.onMouseUp decodeMouseUp
                    , onWheel OnWheel
                    ]

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
