module Meshes exposing (..)

import GBL.Decode exposing (Coordinates)
import Scene3d.Mesh


type alias Meshes =
    { tile : List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
    , spawn : List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
    , straight : List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
    , corner : List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
    , end : List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
    }
