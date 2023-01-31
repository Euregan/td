module Building exposing (..)

import Circle2d exposing (Circle2d)
import Coordinates exposing (GameCoordinates)
import Effect exposing (Effect(..))
import Enemy exposing (Enemy)
import Length
import Meshes exposing (Meshes)
import Point2d
import Point3d
import Scene3d
import Vector3d


type Blueprint
    = OrcTower


type alias Building =
    { blueprint : Blueprint
    , position : ( Int, Int )
    , effect : Effect
    }


init : Blueprint -> ( Int, Int ) -> Building
init blueprint position =
    case blueprint of
        OrcTower ->
            { blueprint = OrcTower
            , position = position
            , effect =
                Damage
                    { range = 3
                    , cooldown = 1000
                    , reloading = 0
                    , damage = 1
                    }
            }


mesh : Meshes -> Blueprint -> List (Scene3d.Entity GameCoordinates)
mesh meshes blueprint =
    case blueprint of
        OrcTower ->
            List.concat
                [ meshes.orcTowerBase
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                , meshes.orcTowerBottom
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                , meshes.orcTowerTop
                    |> List.map (\( m, material, shadow ) -> Scene3d.meshWithShadow material m shadow)
                    |> List.map (Scene3d.translateBy (Vector3d.xyz (Length.meters 0) (Length.meters 0.5) (Length.meters 0)))
                ]
                |> List.map (Scene3d.scaleAbout (Point3d.xyz (Length.meters 0) (Length.meters 0) (Length.meters 0)) 0.8)


tick : Float -> List Enemy -> Building -> ( Building, List Enemy )
tick delta enemies building =
    let
        tickedEffect =
            Effect.tick delta building.effect

        ( effect, updatedEnemies ) =
            case tickedEffect of
                Damage damageEffect ->
                    if damageEffect.reloading == 0 then
                        ( Damage { damageEffect | reloading = damageEffect.cooldown }
                        , List.foldl
                            (\enemy ( previousEnemies, shotFired ) ->
                                if not shotFired && enemyInRange building enemy then
                                    ( { enemy | currentHp = enemy.currentHp - damageEffect.damage } :: previousEnemies, True )

                                else
                                    ( enemy :: previousEnemies, False )
                            )
                            ( [], False )
                            enemies
                            |> Tuple.first
                        )

                    else
                        ( tickedEffect, enemies )
    in
    ( { building | effect = effect }, updatedEnemies )


enemyInRange : Building -> Enemy -> Bool
enemyInRange building enemy =
    case building.effect of
        Damage { range } ->
            Circle2d.contains enemy.position <|
                Circle2d.atPoint
                    (Point2d.xy (Length.meters <| toFloat <| Tuple.first building.position) (Length.meters <| toFloat <| Tuple.second building.position))
                    (Length.meters range)
