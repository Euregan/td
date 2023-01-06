module Meshes exposing (..)

import Coordinates exposing (GameCoordinates)
import Scene3d.Mesh


type alias Meshes =
    { tile : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    , spawn : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    , straight : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    , corner : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    , end : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    , orcTower : List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Mesh.Shadow GameCoordinates )
    }
