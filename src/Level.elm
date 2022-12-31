module Level exposing (..)

import Color
import GBL.Decode exposing (Coordinates)
import Meshes exposing (Meshes)
import Scene3d
import Scene3d.Material
import Vector3d exposing (Vector3d)


type alias Level =
    { width : Int
    , height : Int
    , start : ( Int, Int )
    }


init : ( Int, Int ) -> Level
init ( width, height ) =
    Level width height ( 1, 1 )


view : Meshes -> Level -> List (Scene3d.Entity Coordinates)
view meshes level =
    let
        tile =
            List.map (Scene3d.mesh (Scene3d.Material.matte Color.green)) meshes.tile
    in
    List.concat [ tile, List.map (Scene3d.translateBy (Vector3d.meters 1 0 0)) tile ]
