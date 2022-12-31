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


getMesh : String -> Cmd Msg
getMesh model =
    Http.get
        { url = model
        , expect =
            GBL.Decode.expectGBL GotMesh
        }


type alias Model =
    Maybe (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : String -> ( Model, Cmd Msg )
init model =
    ( Nothing, getMesh model )


type Msg
    = GotMesh (Result Http.Error (List (Scene3d.Mesh.Textured Coordinates)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMesh result ->
            ( Just result, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            text "\u{1F914}"

        Just (Ok meshes) ->
            Scene3d.unlit
                { dimensions = ( Pixels.int 500, Pixels.int 500 )
                , camera =
                    Camera3d.perspective
                        { -- Camera is at the point (4, 2, 2), looking at the point
                          -- (0, 0, 0), oriented so that positive Z appears up
                          viewpoint =
                            Viewpoint3d.lookAt
                                { focalPoint = Point3d.origin
                                , eyePoint = Point3d.meters 4 2 2
                                , upDirection = Direction3d.positiveZ
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
                    List.map (Scene3d.mesh (Scene3d.Material.color Color.blue)) meshes
                }

        Just (Err (Http.BadBody error)) ->
            text error

        Just (Err error) ->
            text "😔"
