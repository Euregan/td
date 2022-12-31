module GBL.Decode exposing (..)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode
import Http
import Json.Decode
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Scene3d.Mesh
import TriangularMesh
import Vector3d exposing (Vector3d)


type MeshPrimitiveMode
    = Points
    | Lines
    | LineLoop
    | LineStrip
    | Triangles
    | TriangleStrip
    | TriangleFan


type alias RawAttributes =
    { position : Int
    , normal : Int
    , textCoord : Int
    }


type alias RawPrimitive =
    { attributes : RawAttributes
    , mode : Int
    , indices : Int
    , material : Int
    }


type alias RawMesh =
    { name : String
    , primitives : Array RawPrimitive
    }


type alias RawBuffer =
    { byteLength : Int
    }


type alias RawBufferView =
    { buffer : Int
    , byteOffset : Int
    , byteLength : Int
    , target : Int
    }


type alias RawAccessor =
    { bufferView : Int
    , byteOffset : Int
    , type_ : String
    , componentType : Int
    , count : Int
    , max : Maybe ( Float, Float, Float )
    , min : Maybe ( Float, Float, Float )
    , normalized : Bool
    }


type alias RawGbl =
    { meshes : Array RawMesh
    , buffers : Array RawBuffer
    , bufferViews : Array RawBufferView
    , accessors : Array RawAccessor
    }


decoder : Json.Decode.Decoder RawGbl
decoder =
    Json.Decode.map4 RawGbl
        (Json.Decode.field "meshes"
            (Json.Decode.array
                (Json.Decode.map2 RawMesh
                    (Json.Decode.field "name" Json.Decode.string)
                    (Json.Decode.field "primitives"
                        (Json.Decode.array
                            (Json.Decode.map4 RawPrimitive
                                (Json.Decode.field "attributes"
                                    (Json.Decode.map3 RawAttributes
                                        (Json.Decode.field "POSITION" Json.Decode.int)
                                        (Json.Decode.field "NORMAL" Json.Decode.int)
                                        (Json.Decode.field "TEXCOORD_0" Json.Decode.int)
                                    )
                                )
                                (Json.Decode.field "mode" Json.Decode.int)
                                (Json.Decode.field "indices" Json.Decode.int)
                                (Json.Decode.field "material" Json.Decode.int)
                            )
                        )
                    )
                )
            )
        )
        (Json.Decode.field "buffers"
            (Json.Decode.array
                (Json.Decode.map RawBuffer
                    (Json.Decode.field "byteLength" Json.Decode.int)
                )
            )
        )
        (Json.Decode.field "bufferViews"
            (Json.Decode.array
                (Json.Decode.map4 RawBufferView
                    (Json.Decode.field "buffer" Json.Decode.int)
                    (Json.Decode.field "byteOffset" Json.Decode.int)
                    (Json.Decode.field "byteLength" Json.Decode.int)
                    (Json.Decode.field "target" Json.Decode.int)
                )
            )
        )
        (Json.Decode.field "accessors"
            (Json.Decode.array
                (Json.Decode.map8 RawAccessor
                    (Json.Decode.field "bufferView" Json.Decode.int)
                    (Json.Decode.field "byteOffset" Json.Decode.int)
                    (Json.Decode.field "type" Json.Decode.string)
                    (Json.Decode.field "componentType" Json.Decode.int)
                    (Json.Decode.field "count" Json.Decode.int)
                    (Json.Decode.maybe <|
                        Json.Decode.field "max" <|
                            Json.Decode.map3 (\x y z -> ( x, y, z ))
                                (Json.Decode.index 0 Json.Decode.float)
                                (Json.Decode.index 1 Json.Decode.float)
                                (Json.Decode.index 2 Json.Decode.float)
                    )
                    (Json.Decode.maybe <|
                        Json.Decode.field "min" <|
                            Json.Decode.map3 (\x y z -> ( x, y, z ))
                                (Json.Decode.index 0 Json.Decode.float)
                                (Json.Decode.index 1 Json.Decode.float)
                                (Json.Decode.index 2 Json.Decode.float)
                    )
                    (Json.Decode.field "normalized" Json.Decode.bool)
                )
            )
        )


type Error
    = Error String
    | JsonError Json.Decode.Error


decodeBytes : Bytes -> Maybe ( RawGbl, Bytes )
decodeBytes =
    Bytes.Decode.string 4
        |> Bytes.Decode.andThen
            (\magic ->
                case magic of
                    "glTF" ->
                        Bytes.Decode.succeed "glTF"

                    _ ->
                        Bytes.Decode.fail
            )
        |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
        |> Bytes.Decode.andThen
            (\version ->
                case version of
                    2 ->
                        Bytes.Decode.succeed 2

                    _ ->
                        Bytes.Decode.fail
            )
        |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
        |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
        |> Bytes.Decode.andThen
            (\length ->
                Bytes.Decode.unsignedInt32 Bytes.LE
                    |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
            )
        |> Bytes.Decode.andThen (\length -> Bytes.Decode.string length)
        |> Bytes.Decode.andThen
            (\json ->
                case Json.Decode.decodeString decoder json of
                    Ok mesh ->
                        Bytes.Decode.unsignedInt32 Bytes.LE
                            |> Bytes.Decode.andThen
                                (\length ->
                                    Bytes.Decode.unsignedInt32 Bytes.LE
                                        |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
                                )
                            |> Bytes.Decode.andThen
                                (\length ->
                                    Bytes.Decode.bytes length
                                )
                            |> Bytes.Decode.andThen (\bytes -> Bytes.Decode.succeed ( mesh, bytes ))

                    Err error ->
                        Bytes.Decode.fail
            )
        |> Bytes.Decode.decode


type alias MeshData =
    { vertices :
        Array
            { position : Point3d Meters Coordinates
            , normal : Vector3d Unitless Coordinates
            , uv : ( Float, Float )
            }
    , indices : List ( Int, Int, Int )
    }


rawToParsed : RawGbl -> Bytes -> Result String (List MeshData)
rawToParsed raw bytes =
    let
        maybeAccessors : List ( Maybe RawAccessor, Maybe RawAccessor, Maybe RawAccessor )
        maybeAccessors =
            Array.map
                (\mesh ->
                    Array.map
                        (\primitive ->
                            ( Array.get primitive.attributes.position raw.accessors
                            , Array.get primitive.attributes.normal raw.accessors
                            , Array.get primitive.indices raw.accessors
                            )
                        )
                        mesh.primitives
                )
                raw.meshes
                |> Array.toList
                |> List.foldl (\acc list -> List.concat [ Array.toList acc, list ]) []

        accessors : Result String (List ( RawAccessor, RawAccessor, RawAccessor ))
        accessors =
            maybeAccessors
                |> List.foldl
                    (\maybePositionAndIndiceAccessor result ->
                        case ( result, maybePositionAndIndiceAccessor ) of
                            ( Just (Err error), _ ) ->
                                Just (Err error)

                            ( Just (Ok acc), ( Just position, Just normal, Just indice ) ) ->
                                Just <| Ok <| ( position, normal, indice ) :: acc

                            ( Nothing, ( Just position, Just normal, Just indice ) ) ->
                                Just <| Ok <| [ ( position, normal, indice ) ]

                            ( _, ( Nothing, _, _ ) ) ->
                                Just (Err "The position accessor could not be found")

                            ( _, ( _, Nothing, _ ) ) ->
                                Just (Err "The normal accessor could not be found")

                            ( _, ( _, _, Nothing ) ) ->
                                Just (Err "The indice accessor could not be found")
                    )
                    Nothing
                |> Maybe.withDefault (Err "The mesh list cannot be empty")

        resultMaybeViews : Result String (List ( ( RawAccessor, Maybe RawBufferView ), ( RawAccessor, Maybe RawBufferView ), ( RawAccessor, Maybe RawBufferView ) ))
        resultMaybeViews =
            accessors
                |> Result.map
                    (List.map
                        (\( position, normal, indice ) ->
                            ( ( position, Array.get position.bufferView raw.bufferViews )
                            , ( normal, Array.get normal.bufferView raw.bufferViews )
                            , ( indice, Array.get indice.bufferView raw.bufferViews )
                            )
                        )
                    )

        views : Result String (List ( ( RawAccessor, RawBufferView ), ( RawAccessor, RawBufferView ), ( RawAccessor, RawBufferView ) ))
        views =
            case resultMaybeViews of
                Err error ->
                    Err error

                Ok maybeViews ->
                    maybeViews
                        |> List.foldl
                            (\maybePositionAndIndiceBufferView result ->
                                case ( result, maybePositionAndIndiceBufferView ) of
                                    ( Just (Err error), _ ) ->
                                        Just (Err error)

                                    ( Just (Ok acc), ( ( positionAccessor, Just position ), ( normalAccessor, Just normal ), ( indiceAccessor, Just indice ) ) ) ->
                                        Just <| Ok <| ( ( positionAccessor, position ), ( normalAccessor, normal ), ( indiceAccessor, indice ) ) :: acc

                                    ( Nothing, ( ( positionAccessor, Just position ), ( normalAccessor, Just normal ), ( indiceAccessor, Just indice ) ) ) ->
                                        Just <| Ok <| [ ( ( positionAccessor, position ), ( normalAccessor, normal ), ( indiceAccessor, indice ) ) ]

                                    ( _, ( ( _, Nothing ), _, _ ) ) ->
                                        Just (Err "The position buffer view could not be found")

                                    ( _, ( _, ( _, Nothing ), _ ) ) ->
                                        Just (Err "The normal buffer view could not be found")

                                    ( _, ( _, _, ( _, Nothing ) ) ) ->
                                        Just (Err "The indice buffer view could not be found")
                            )
                            Nothing
                        |> Maybe.withDefault (Err "The accessor list cannot be empty")

        resultMaybeData : Result String (List ( Maybe (List ( Float, Float, Float )), Maybe (List ( Float, Float, Float )), Maybe (List ( Int, Int, Int )) ))
        resultMaybeData =
            case views of
                Err error ->
                    Err error

                Ok bufferViews ->
                    bufferViews
                        |> List.map
                            (\( ( positionAccessor, position ), ( normalAccessor, normal ), ( indiceAccessor, indice ) ) ->
                                ( bytes
                                    |> Bytes.Decode.decode
                                        (Bytes.Decode.bytes position.byteOffset
                                            |> Bytes.Decode.andThen
                                                (\_ ->
                                                    Bytes.Decode.loop ( 0, [] )
                                                        (\( done, vertices ) ->
                                                            if done < positionAccessor.count then
                                                                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + 1, vector :: vertices ))

                                                            else
                                                                Bytes.Decode.Done vertices
                                                                    |> Bytes.Decode.succeed
                                                        )
                                                )
                                        )
                                , bytes
                                    |> Bytes.Decode.decode
                                        (Bytes.Decode.bytes normal.byteOffset
                                            |> Bytes.Decode.andThen
                                                (\_ ->
                                                    Bytes.Decode.loop ( 0, [] )
                                                        (\( done, vertices ) ->
                                                            if done < normalAccessor.count then
                                                                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    (Bytes.Decode.float32 Bytes.LE)
                                                                    |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + 1, vector :: vertices ))

                                                            else
                                                                Bytes.Decode.Done vertices
                                                                    |> Bytes.Decode.succeed
                                                        )
                                                )
                                        )
                                , bytes
                                    |> Bytes.Decode.decode
                                        (Bytes.Decode.bytes indice.byteOffset
                                            |> Bytes.Decode.andThen
                                                (\_ ->
                                                    Bytes.Decode.loop ( 0, [] )
                                                        (\( done, vertices ) ->
                                                            if done < indiceAccessor.count then
                                                                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                                                                    (Bytes.Decode.unsignedInt32 Bytes.LE)
                                                                    (Bytes.Decode.unsignedInt32 Bytes.LE)
                                                                    (Bytes.Decode.unsignedInt32 Bytes.LE)
                                                                    |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + 3, vector :: vertices ))

                                                            else
                                                                Bytes.Decode.Done vertices
                                                                    |> Bytes.Decode.succeed
                                                        )
                                                )
                                        )
                                )
                            )
                        |> Ok

        data : Result String (List ( List ( Float, Float, Float ), List ( Float, Float, Float ), List ( Int, Int, Int ) ))
        data =
            case resultMaybeData of
                Err error ->
                    Err error

                Ok maybeDatalist ->
                    maybeDatalist
                        |> List.foldl
                            (\maybeData result ->
                                case ( result, maybeData ) of
                                    ( Just (Err error), _ ) ->
                                        Just <| Err error

                                    ( Just (Ok list), ( Just positions, Just normals, Just indices ) ) ->
                                        Just <| Ok <| ( positions, normals, indices ) :: list

                                    ( Nothing, ( Just positions, Just normals, Just indices ) ) ->
                                        Just <| Ok <| [ ( positions, normals, indices ) ]

                                    ( _, ( Nothing, _, _ ) ) ->
                                        Just <| Err "The positions failed to be parsed"

                                    ( _, ( _, Nothing, _ ) ) ->
                                        Just <| Err "The normals failed to be parsed"

                                    ( _, ( _, _, Nothing ) ) ->
                                        Just <| Err "The indices failed to be parsed"
                            )
                            Nothing
                        |> Maybe.withDefault (Err "The data cannot be empty")
    in
    Result.map
        (List.map
            (\( position, normal, indices ) ->
                MeshData
                    (List.map2
                        (\( posX, posY, posZ ) ( normX, normY, normZ ) ->
                            { position = Point3d.meters posX posY posZ
                            , normal = Vector3d.unitless normX normY normZ
                            , uv = ( 0, 0 )
                            }
                        )
                        (position |> List.reverse)
                        (normal |> List.reverse)
                        |> Array.fromList
                    )
                    indices
            )
        )
        data


type Coordinates
    = Coordinates


parsedToMesh : List MeshData -> List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )
parsedToMesh =
    List.map
        (\data ->
            let
                mesh =
                    TriangularMesh.indexed data.vertices data.indices
                        |> Scene3d.Mesh.texturedFaces

                shadow =
                    Scene3d.Mesh.shadow mesh
            in
            ( mesh, shadow )
        )


expectGBL : (Result Http.Error (List ( Scene3d.Mesh.Textured Coordinates, Scene3d.Mesh.Shadow Coordinates )) -> msg) -> Http.Expect msg
expectGBL toMsg =
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
                    case decodeBytes body of
                        Just ( mesh, bytes ) ->
                            rawToParsed mesh bytes
                                |> Result.map parsedToMesh
                                |> Result.mapError Http.BadBody

                        Nothing ->
                            Err <| Http.BadBody "Decoding the file failed"
