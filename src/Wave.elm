module Wave exposing (..)

import Camera exposing (Camera)
import Coordinates exposing (GameCoordinates)
import Enemy exposing (Enemy)
import Html exposing (Html)
import Level exposing (Level)
import Meshes exposing (Meshes)
import Random exposing (Seed)
import Scene3d
import Screen exposing (Screen)
import Vector3d


type alias Wave =
    { enemies : List Enemy
    }


init : Seed -> Level -> ( Wave, Seed )
init seed level =
    let
        ( enemies, finalSeed ) =
            List.repeat 10 seed
                |> List.foldr
                    (\_ ( initializedEnemies, s ) ->
                        let
                            ( enemy, newSeed ) =
                                Enemy.init s level
                        in
                        ( enemy :: initializedEnemies, newSeed )
                    )
                    ( [], seed )
    in
    ( { enemies = enemies
      }
    , finalSeed
    )


tick : Float -> Wave -> Wave
tick delta wave =
    { wave | enemies = List.map (Enemy.tick delta) wave.enemies }


view : ( Camera, Screen ) -> Meshes -> Wave -> Level -> List ( Scene3d.Entity GameCoordinates, Html msg )
view ( camera, screen ) meshes wave level =
    List.map (Enemy.view ( camera, screen, level ) meshes) wave.enemies
