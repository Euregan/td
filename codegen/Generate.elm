module Generate exposing (main)

import Elm
import Elm.Annotation
import Elm.Case
import Gen.CodeGen.Generate as Generate
import Gen.Helper
import Gen.Maybe
import Gen.Result


assets =
    [ "tile"
    , "spawn"
    , "straight"
    , "corner"
    , "end"
    , "orcTowerBase"
    , "orcTowerBottom"
    , "orcTowerMiddle"
    , "orcTowerTop"
    , "characterSkeleton"
    ]


main : Program {} () ()
main =
    Generate.run
        [ meshes
        , loading
        , flags
        ]


meshes : Elm.File
meshes =
    Elm.file [ "Meshes" ]
        [ Elm.alias "Meshes" <|
            Elm.Annotation.record <|
                List.map
                    (\asset ->
                        ( asset
                        , Elm.Annotation.list <| Elm.Annotation.triple meshAnnotation materialAnnotation shadowAnnotation
                        )
                    )
                    assets
        ]


meshAnnotation =
    Elm.Annotation.namedWith [ "Scene3d.Mesh" ] "Textured" [ Elm.Annotation.named [ "Coordinates" ] "GameCoordinates" ]

materialAnnotation =
    Elm.Annotation.namedWith [ "Scene3d", "Material" ] "Material" [ Elm.Annotation.named [ "Coordinates" ] "GameCoordinates", Elm.Annotation.record [ ( "normals", Elm.Annotation.unit ), ( "uvs", Elm.Annotation.unit ) ] ]

shadowAnnotation =
    Elm.Annotation.namedWith [ "Scene3d.Mesh" ] "Shadow" [ Elm.Annotation.named [ "Coordinates" ] "GameCoordinates" ]


loadingAnnotation =
    Elm.Annotation.maybe <|
        Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <|
            Elm.Annotation.list <|
                Elm.Annotation.triple meshAnnotation materialAnnotation shadowAnnotation


loading : Elm.File
loading =
    Elm.file [ "Loading" ]
        [ Elm.alias "Loading" <|
            Elm.Annotation.record <|
                List.map
                    (\asset -> ( asset, loadingAnnotation ))
                    assets
        , Elm.customType "Msg" <|
            List.map
                (\asset ->
                    Elm.variantWith ("Got" ++ capitalize asset)
                        [ Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <|
                            Elm.Annotation.list <|
                                Elm.Annotation.triple meshAnnotation materialAnnotation shadowAnnotation
                        ]
                )
                assets
        , Elm.declaration "init"
            ((Elm.record <| List.map (\asset -> ( asset, Elm.nothing )) assets) |> Elm.withType (Elm.Annotation.named [] "Loading"))
        , Elm.declaration "update"
            (Elm.fn2
                ( "loading", Just <| Elm.Annotation.named [] "Loading" )
                ( "msg", Just <| Elm.Annotation.named [] "Msg" )
                (\model msg ->
                    Elm.Case.custom
                        msg
                        (Elm.Annotation.named [] "Msg")
                    <|
                        List.map
                            (\asset ->
                                Elm.Case.branch1 ("Got" ++ capitalize asset)
                                    ( "result", Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.named [ "Meshes" ] "Meshes" )
                                    (\mesh -> Elm.updateRecord [ ( asset, Elm.just mesh ) ] model)
                            )
                            assets
                )
            )
        , Elm.declaration "toMeshes"
            (Elm.fn ( "loading", Just <| Elm.Annotation.named [] "Loading" )
                (tryToConvert [] assets)
                |> Elm.withType
                    (Elm.Annotation.function
                        [ Elm.Annotation.named [] "Loading" ]
                        (Elm.Annotation.maybe <| Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.named [ "Meshes" ] "Meshes")
                    )
            )
        , Elm.declaration "getMesh"
            (Elm.fn2
                ( "model", Just Elm.Annotation.string )
                ( "msg"
                , Just
                    (Elm.Annotation.function
                        [ Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.list <| Elm.Annotation.triple meshAnnotation materialAnnotation shadowAnnotation ]
                        (Elm.Annotation.named [] "Msg")
                    )
                )
                (\model msg ->
                    Elm.apply (Elm.value { importFrom = [ "Http" ], name = "get", annotation = Nothing })
                        [ Elm.record
                            [ ( "url", model )
                            , ( "expect"
                              , Elm.apply (Elm.value { importFrom = [ "GBL", "Decode" ], name = "expectGltf", annotation = Nothing })
                                    [ Elm.apply (Elm.value { importFrom = [ "Msg" ], name = "GotMesh", annotation = Nothing }) [ msg ]
                                    ]
                              )
                            ]
                        ]
                )
                |> Elm.withType
                    (Elm.Annotation.function
                        [ Elm.Annotation.string
                        , Elm.Annotation.function
                            [ Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.list <| Elm.Annotation.triple meshAnnotation materialAnnotation shadowAnnotation ]
                            (Elm.Annotation.named [] "Msg")
                        ]
                        (Elm.Annotation.namedWith
                            []
                            "Cmd"
                            [ Elm.Annotation.namedWith [ "Msg" ] "Msg" [ Elm.Annotation.named [] "Msg" ] ]
                        )
                    )
            )
        , Elm.declaration "commands"
            (Elm.fn
                ( "models", Nothing )
                (\models ->
                    -- Loading.getMesh models.tile Loading.GotTile
                    Elm.list <|
                        List.map
                            (\asset ->
                                Elm.apply
                                    (Elm.value { importFrom = [], name = "getMesh", annotation = Nothing })
                                    [ Elm.get asset models
                                    , Elm.val ("Got" ++ capitalize asset)
                                    ]
                            )
                            assets
                )
                |> Elm.withType
                    (Elm.Annotation.function [ Elm.Annotation.named [ "Flags" ] "Meshes" ]
                        (Elm.Annotation.namedWith
                            []
                            "List"
                            [ Elm.Annotation.namedWith
                                []
                                "Cmd"
                                [ Elm.Annotation.namedWith
                                    [ "Msg" ]
                                    "Msg"
                                    [ Elm.Annotation.named [] "Msg" ]
                                ]
                            ]
                        )
                    )
            )
        ]


tryToConvert : List ( String, Elm.Expression ) -> List String -> Elm.Expression -> Elm.Expression
tryToConvert pastAssets list meshLoadingState =
    case list of
        asset :: remaining ->
            Elm.Case.custom
                (Elm.get asset meshLoadingState)
                (Elm.Annotation.maybe <| Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.named [ "Meshes" ] "Meshes")
                [ Elm.Case.branch0 "Nothing" Elm.nothing
                , Elm.Case.branch1 "Just"
                    ( "result", Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.named [ "Meshes" ] "Meshes" )
                    (\result ->
                        Elm.Case.custom
                            result
                            (Elm.Annotation.result (Elm.Annotation.named [ "Http" ] "Error") <| Elm.Annotation.named [ "Meshes" ] "Meshes")
                            [ Elm.Case.branch1 "Err" ( "error", Elm.Annotation.named [ "Http" ] "Error" ) (\error -> Elm.just <| err error)
                            , Elm.Case.branch1 "Ok"
                                ( "mesh", Elm.Annotation.named [ "Meshes" ] "Meshes" )
                                (\mesh -> tryToConvert (( asset, mesh ) :: pastAssets) remaining meshLoadingState)
                            ]
                    )
                ]

        [] ->
            Elm.just <| ok <| Elm.record pastAssets


ok result =
    Elm.apply (Elm.value { importFrom = [ "Result" ], name = "Ok", annotation = Nothing }) [ result ]


err error =
    Elm.apply (Elm.value { importFrom = [ "Result" ], name = "Err", annotation = Nothing }) [ error ]


capitalize text =
    case String.uncons text of
        Just ( head, rest ) ->
            String.cons (Char.toUpper head) rest

        Nothing ->
            ""


flags : Elm.File
flags =
    Elm.file [ "Flags" ]
        [ Elm.alias "Meshes" <|
            Elm.Annotation.record <|
                List.map
                    (\asset -> ( asset, Elm.Annotation.string ))
                    assets
        , Elm.alias "Flags" <|
            Elm.Annotation.record <|
                [ ( "viewport", Elm.Annotation.named [ "Viewport" ] "Viewport" )
                , ( "models", Elm.Annotation.named [] "Meshes" )
                , ( "seed", Elm.Annotation.named [] "Int" )
                ]
        ]
