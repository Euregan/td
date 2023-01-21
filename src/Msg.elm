module Msg exposing (..)

import Coordinates
import Http
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Scene3d.Material
import Scene3d.Mesh
import Viewport exposing (Viewport)


type alias MeshResult =
    Result Http.Error (List ( Scene3d.Mesh.Textured Coordinates.GameCoordinates, Scene3d.Material.Material Coordinates.GameCoordinates { normals : (), uvs : () }, Scene3d.Mesh.Shadow Coordinates.GameCoordinates ))


type Msg msg
    = GotMesh (MeshResult -> msg) MeshResult
    | WindowResized Viewport
    | MouseMoved (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseDown (Quantity Float Pixels) (Quantity Float Pixels)
    | NewFrame Float
