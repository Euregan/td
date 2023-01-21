module Building exposing (..)

import Color
import Coordinates exposing (GameCoordinates)
import Meshes exposing (Meshes)
import Scene3d
import Scene3d.Material


type Blueprint
    = OrcTower


type alias Building =
    { blueprint : Blueprint
    , position : ( Int, Int )
    }


mesh : Meshes -> Blueprint -> List (Scene3d.Entity GameCoordinates)
mesh meshes blueprint =
    case blueprint of
        OrcTower ->
            meshes.orcTower
                |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
