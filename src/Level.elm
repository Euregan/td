module Level exposing (..)

import Color
import GBL.Decode exposing (Coordinates)
import Meshes exposing (Meshes)
import Scene3d
import Scene3d.Material
import Vector3d


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
    List.range 0 level.width
        |> List.map (\x -> toFloat x - toFloat level.width / 2)
        |> List.map
            (\x ->
                List.range 0 level.height
                    |> List.map (\y -> toFloat y - toFloat level.height / 2)
                    |> List.map
                        (\y ->
                            List.map (Scene3d.translateBy (Vector3d.meters x 0 y)) tile
                        )
            )
        |> List.concat
        |> List.concat
