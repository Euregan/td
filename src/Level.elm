module Level exposing (..)

import AStar exposing (Path, Position)
import Angle
import Array
import Axis3d
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
            List.map (\( mesh, material, shadow ) -> Scene3d.meshWithShadow material mesh shadow) meshes.tile

        spawn =
            List.map (\( mesh, material, shadow ) -> Scene3d.meshWithShadow material mesh shadow) meshes.spawn

        end =
            List.map (\( mesh, material, shadow ) -> Scene3d.meshWithShadow material mesh shadow) meshes.end

        straight =
            List.map (\( mesh, material, shadow ) -> Scene3d.meshWithShadow material mesh shadow) meshes.straight

        corner =
            List.map (\( mesh, material, shadow ) -> Scene3d.meshWithShadow material mesh shadow) meshes.corner

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
        |> pathMap
            (\maybePrevious ( x, y ) maybeNext ->
                position
                    (case ( maybePrevious, maybeNext ) of
                        ( Nothing, Just ( nextX, nextY ) ) ->
                            if nextX > x then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 270))

                            else if nextX < x then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                            else if nextY > y then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 180))

                            else
                                spawn

                        ( Just ( previousX, previousY ), Nothing ) ->
                            if previousX > x then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 270))

                            else if previousX < x then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                            else if previousY > y then
                                spawn |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 180))

                            else
                                spawn

                        ( Just previous, Just next ) ->
                            case ( positionsToDirection ( x, y ) previous, positionsToDirection ( x, y ) next ) of
                                ( Above, Below ) ->
                                    straight

                                ( Below, Above ) ->
                                    straight

                                ( Left, Right ) ->
                                    straight |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                                ( Right, Left ) ->
                                    straight |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                                ( Above, Right ) ->
                                    corner

                                ( Right, Above ) ->
                                    corner

                                ( Above, Left ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 270))

                                ( Left, Above ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 270))

                                ( Below, Right ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                                ( Right, Below ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 90))

                                ( Below, Left ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 180))

                                ( Left, Below ) ->
                                    corner |> List.map (Scene3d.rotateAround Axis3d.y (Angle.degrees 180))

                                -- This should never happen
                                _ ->
                                    []

                        _ ->
                            tile
                    )
                    x
                    y
            )
            level.path


type Direction
    = Left
    | Right
    | Above
    | Below


{-| Only works for two neighbor positions, otherwise the result will be Below
-}
positionsToDirection : Position -> Position -> Direction
positionsToDirection ( originX, originY ) ( positionX, positionY ) =
    if originY == positionY then
        if originX > positionX then
            Right

        else
            Left

    else if originY > positionY then
        Above

    else
        Below


pathMap : (Maybe Position -> Position -> Maybe Position -> List (Scene3d.Entity GameCoordinates)) -> Path -> List Position -> List (Scene3d.Entity GameCoordinates)
pathMap func p positions =
    let
        array =
            Array.fromList p
    in
    List.concat
        [ List.filter (\( x, y ) -> List.all (\( pathX, pathY ) -> pathX /= x || pathY /= y) p) positions
            |> List.map (\position -> func Nothing position Nothing)
            |> List.concat
        , List.indexedMap (\index position -> func (Array.get (index - 1) array) position (Array.get (index + 1) array)) p
            |> List.concat
        ]


tiles : Level -> List Position
tiles level =
    List.range 0 (level.width - 1)
        |> List.map (\x -> List.range 0 (level.length - 1) |> List.map (\y -> ( x, y )))
        |> List.concat
