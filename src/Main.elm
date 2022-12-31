module Main exposing (..)

import Angle
import Browser
import Camera3d
import Color
import Direction3d
import GBL.Decode exposing (Coordinates)
import Html exposing (Html, text)
import Http
import Length
import Level exposing (Level)
import Pixels
import Point3d
import Scene3d
import Scene3d.Material
import Scene3d.Mesh
import Viewpoint3d


getMesh : String -> (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)) -> Msg) -> Cmd Msg
getMesh model msg =
    Http.get
        { url = model
        , expect =
            GBL.Decode.expectGBL msg
        }


type alias LoadingMeshes =
    { tile : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates))), spawn : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates))), straight : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates))), corner : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates))), end : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates))) }


type alias GameState =
    { level : Level
    , meshes :
        { tile : List (Scene3d.Mesh.Textured Coordinates)
        , spawn : List (Scene3d.Mesh.Textured Coordinates)
        , straight : List (Scene3d.Mesh.Textured Coordinates)
        , corner : List (Scene3d.Mesh.Textured Coordinates)
        , end : List (Scene3d.Mesh.Textured Coordinates)
        }
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
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    { tile : String
    , spawn : String
    , straight : String
    , corner : String
    , end : String
    }


init : Flags -> ( Model, Cmd Msg )
init models =
    ( Loading
        { tile = Nothing
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
    = GotTile (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    | GotSpawn (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    | GotStraight (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    | GotCorner (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    | GotEnd (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading meshes ->
            let
                checkState : LoadingMeshes -> Model
                checkState { tile, spawn, straight, corner, end } =
                    case [ tile, spawn, straight, corner, end ] of
                        [ Just (Ok loadedTile), Just (Ok loadedSpawn), Just (Ok loadedStraight), Just (Ok loadedCorner), Just (Ok loadedEnd) ] ->
                            Loaded
                                { level = Level.init ( 10, 10 )
                                , meshes =
                                    { tile = loadedTile
                                    , spawn = loadedSpawn
                                    , straight = loadedStraight
                                    , corner = loadedCorner
                                    , end = loadedEnd
                                    }
                                }

                        [ Nothing, _, _, _, _ ] ->
                            Loading { tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, Nothing, _, _, _ ] ->
                            Loading { tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, Nothing, _, _ ] ->
                            Loading { tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, _, Nothing, _ ] ->
                            Loading { tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

                        [ _, _, _, _, Nothing ] ->
                            Loading { tile = tile, spawn = spawn, straight = straight, corner = corner, end = end }

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

        Loaded _ ->
            ( model, Cmd.none )

        Error ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "\u{1F914}"

        Loaded { level, meshes } ->
            Scene3d.sunny
                { dimensions = ( Pixels.int 500, Pixels.int 500 )
                , camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { focalPoint = Point3d.origin
                                , eyePoint = Point3d.meters 4 2 2
                                , upDirection = Direction3d.positiveY
                                }
                        , verticalFieldOfView = Angle.degrees 20
                        }
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , entities =
                    List.map (Scene3d.mesh (Scene3d.Material.matte Color.green)) meshes.tile
                , shadows = True
                , sunlightDirection = Direction3d.yx <| Angle.degrees 140
                , upDirection = Direction3d.positiveY
                }

        Error ->
            text "😔"
