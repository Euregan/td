module Meshes exposing (..)

import GBL.Decode exposing (Coordinates)
import Scene3d.Mesh


type alias Meshes =
    { tile : List (Scene3d.Mesh.Textured Coordinates)
    , spawn : List (Scene3d.Mesh.Textured Coordinates)
    , straight : List (Scene3d.Mesh.Textured Coordinates)
    , corner : List (Scene3d.Mesh.Textured Coordinates)
    , end : List (Scene3d.Mesh.Textured Coordinates)
    }
