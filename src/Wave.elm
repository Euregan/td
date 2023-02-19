module Wave exposing (..)

import Coordinates exposing (GameCoordinates)
import Enemy exposing (Enemy)
import Level exposing (Level)
import Meshes exposing (Meshes)
import Random exposing (Seed)
import Scene3d
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


view : Meshes -> Wave -> Level -> List (Scene3d.Entity GameCoordinates)
view meshes wave level =
    List.map (Enemy.view meshes) wave.enemies
        |> List.map
            (Scene3d.translateBy
                (Vector3d.meters
                    (toFloat level.width / -2)
                    0
                    (toFloat level.length / -2)
                )
            )
