module Level exposing (..)

import Color
import GBL.Decode exposing (Coordinates)
import Meshes exposing (Meshes)
import Scene3d
import Scene3d.Material
import Vector3d


type alias Level =
    { width : Int
    , length : Int
    , start : { x : Int, y : Int }
    , end : { x : Int, y : Int }
    }


init : ( Int, Int ) -> Level
init ( width, length ) =
    Level
        width
        length
        { x = width // 2, y = 1 }
        { x = width // 2, y = length - 1 }


view : Meshes -> Level -> List (Scene3d.Entity Coordinates)
view meshes level =
    let
        tile =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.tile

        spawn =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.spawn

        end =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.end

        position : List (Scene3d.Entity Coordinates) -> Int -> Int -> List (Scene3d.Entity Coordinates)
        position mesh x y =
            List.map (Scene3d.translateBy (Vector3d.meters (toFloat x - toFloat level.width / 2) 0 (toFloat y - toFloat level.length / 2))) mesh
    in
    List.range 0 level.width
        |> List.map
            (\x ->
                List.range 0 level.length
                    |> List.map
                        (\y ->
                            position
                                (if x == level.start.x && y == level.start.y then
                                    spawn

                                 else if x == level.end.x && y == level.end.y then
                                    end

                                 else
                                    tile
                                )
                                x
                                y
                        )
            )
        |> List.concat
        |> List.concat
