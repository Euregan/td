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
    case ( player.selected, hovered ) of
        ( Just building, Just ( x, y ) ) ->
            Building.mesh meshes building
                |> List.map
                    (Scene3d.translateBy (Vector3d.meters (toFloat x) 0.3 (toFloat y)))

        _ ->
            []
