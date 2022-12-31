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


type alias Model =
    { tile : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    , spawn : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    , straight : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    , corner : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    , end : Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))
    }


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
    ( { tile = Nothing
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
    case msg of
        GotTile mesh ->
            ( { model | tile = Just mesh }, Cmd.none )

        GotSpawn mesh ->
            ( { model | spawn = Just mesh }, Cmd.none )

        GotStraight mesh ->
            ( { model | straight = Just mesh }, Cmd.none )

        GotCorner mesh ->
            ( { model | corner = Just mesh }, Cmd.none )

        GotEnd mesh ->
            ( { model | end = Just mesh }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.tile of
        Nothing ->
            text "\u{1F914}"

        Just (Ok meshes) ->
            Scene3d.sunny
                { dimensions = ( Pixels.int 500, Pixels.int 500 )
                , camera =
                    Camera3d.perspective
                        { -- Camera is at the point (4, 2, 2), looking at the point
                          -- (0, 0, 0), oriented so that positive Z appears up
                          viewpoint =
                            Viewpoint3d.lookAt
                                { focalPoint = Point3d.origin
                                , eyePoint = Point3d.meters 4 2 2
                                , upDirection = Direction3d.positiveY
                                }

                        -- The image on the screen will have a total rendered 'height'
                        -- of 30 degrees; small angles make the camera act more like a
                        -- telescope and large numbers make it act more like a fisheye
                        -- lens
                        , verticalFieldOfView = Angle.degrees 30
                        }
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , entities =
                    List.map (Scene3d.mesh (Scene3d.Material.matte Color.green)) meshes
                , shadows = True
                , sunlightDirection = Direction3d.yx <| Angle.degrees 140
                , upDirection = Direction3d.positiveY
                }

        Just (Err (Http.BadBody error)) ->
            text error

        Just (Err error) ->
            text "😔"
