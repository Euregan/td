module GBL.Decode exposing (expectGltf)

import Color exposing (Color)
import Coordinates exposing (GameCoordinates)
import Gltf.Decode.Mesh
import Http
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Scene3d.Material
import Scene3d.Mesh
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


expectGltf : (Result Http.Error (List ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Material.Material GameCoordinates { normals : (), uvs : () }, Scene3d.Mesh.Shadow GameCoordinates )) -> msg) -> Http.Expect msg
expectGltf toMsg =
    Http.expectBytesResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    Gltf.Decode.Mesh.texturedFacesFromDefaultScene body
                        |> Result.map (List.map toMesh)
                        |> Result.mapError Http.BadBody


toMesh : ( TriangularMesh { position : Point3d Meters GameCoordinates, normal : Vector3d Unitless GameCoordinates, uv : ( Float, Float ) }, Color ) -> ( Scene3d.Mesh.Textured GameCoordinates, Scene3d.Material.Material GameCoordinates { normals : (), uvs : () }, Scene3d.Mesh.Shadow GameCoordinates )
toMesh ( triangles, color ) =
    let
        mesh =
            Scene3d.Mesh.texturedFaces triangles

        shadow =
            Scene3d.Mesh.shadow mesh

        material =
            Scene3d.Material.texturedPbr
                { baseColor = Scene3d.Material.constant color
                , roughness = Scene3d.Material.constant 1
                , metallic = Scene3d.Material.constant 1
                }
    in
    ( mesh, material, shadow )
