module Level exposing (..)

import AStar exposing (Path, Position)
import Color
import Coordinates exposing (GameCoordinates)
import Meshes exposing (Meshes)
import Scene3d
import Scene3d.Material
import Set
import Vector3d


type alias Level =
    { width : Int
    , length : Int
    , start : { x : Int, y : Int }
    , end : { x : Int, y : Int }
    , path : Path
    }


type Msg
    = BuildingsChanged (List ( Int, Int ))


init : ( Int, Int ) -> Level
init ( width, length ) =
    let
        start =
            { x = width // 2, y = 1 }

        end =
            { x = width // 2, y = length - 2 }
    in
    { width = width
    , length = length
    , start = start
    , end = end
    , path = path width length start end []
    }


path : Int -> Int -> { x : Int, y : Int } -> { x : Int, y : Int } -> List ( Int, Int ) -> Path
path width length start end obstacles =
    AStar.findPath AStar.straightLineCost
        (\( x, y ) ->
            [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
                -- Filtering out positions outside the level
                |> List.filter (\( x_, y_ ) -> x_ >= 0 && y_ >= 0 && x_ < width && y_ < length)
                -- Filtering out occupied positions
                |> List.filter (\( targetX, targetY ) -> List.all (\( obstacleX, obstacleY ) -> targetX /= obstacleX || targetY /= obstacleY) obstacles)
                |> Set.fromList
        )
        ( start.x, start.y )
        ( end.x, end.y )
        |> Maybe.withDefault []


update : Level -> Msg -> Level
update level msg =
    case msg of
        BuildingsChanged buildings ->
            { level | path = path level.width level.length level.start level.end buildings }


view : Meshes -> Level -> Maybe Position -> List (Scene3d.Entity GameCoordinates)
view meshes level hoveredTile =
    let
        tile =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.tile

        spawn =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.spawn

        end =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.end

        straight =
            List.map (\( mesh, shadow ) -> Scene3d.meshWithShadow (Scene3d.Material.matte Color.green) mesh shadow) meshes.straight

        position : List (Scene3d.Entity GameCoordinates) -> Int -> Int -> List (Scene3d.Entity GameCoordinates)
        position mesh x y =
            List.map
                (Scene3d.translateBy
                    (Vector3d.meters
                        (toFloat x - toFloat level.width / 2)
                        0
                        (toFloat y - toFloat level.length / 2)
                    )
                )
                mesh
    in
    tiles level
        |> List.map
            (\( x, y ) ->
                position
                    (if x == level.start.x && y == level.start.y then
                        spawn

                     else if x == level.end.x && y == level.end.y then
                        end

                     else if List.member ( x, y ) level.path then
                        straight

                     else
                        tile
                    )
                    x
                    y
            )
        |> List.concat


tiles : Level -> List Position
tiles level =
    List.range 0 (level.width - 1)
        |> List.map (\x -> List.range 0 (level.length - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
