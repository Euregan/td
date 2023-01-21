module Building exposing (..)

import Coordinates exposing (GameCoordinates)
import Length
import Meshes exposing (Meshes)
import Point3d
import Scene3d
import Vector3d


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
            List.concat
                [ meshes.orcTowerBase
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                , meshes.orcTowerBottom
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                , meshes.orcTowerTop
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                    |> List.map (Scene3d.translateBy (Vector3d.xyz (Length.meters 0) (Length.meters 0.5) (Length.meters 0)))
                ]
                |> List.map (Scene3d.scaleAbout (Point3d.xyz (Length.meters 0) (Length.meters 0) (Length.meters 0)) 0.8)
