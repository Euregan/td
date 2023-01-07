module Player exposing (..)

import AStar exposing (Position)
import Building exposing (Blueprint(..), Building)
import Coordinates exposing (GameCoordinates)
import Meshes exposing (Meshes)
import Scene3d
import Vector3d


type alias Player =
    { buildings : List Building
    , selected : Maybe Blueprint
    }


init : Player
init =
    { buildings = []
    , selected = Just OrcTower
    }


view : Meshes -> Player -> Maybe Position -> List (Scene3d.Entity GameCoordinates)
view meshes player hovered =
    let
        buildings =
            player.buildings
                |> List.map
                    (\building ->
                        Building.mesh meshes building.blueprint
                            |> List.map (Scene3d.translateBy (Vector3d.meters (toFloat <| Tuple.first building.position) 0.3 (toFloat <| Tuple.second building.position)))
                    )
                |> List.concat
    in
    case ( player.selected, hovered ) of
        ( Just blueprint, Just ( x, y ) ) ->
            List.concat
                [ Building.mesh meshes blueprint
                    |> List.map
                        (Scene3d.translateBy (Vector3d.meters (toFloat x) 0.3 (toFloat y)))
                , buildings
                ]

        _ ->
            buildings


build : Player -> ( Int, Int ) -> Player
build player position =
    case player.selected of
        Nothing ->
            player

        Just blueprint ->
            { player | buildings = { blueprint = blueprint, position = position } :: player.buildings }
