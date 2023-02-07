module Effect exposing (..)


type Effect
    = Damage
        { range : Float
        , cooldown : Float
        , reloading : Float
        , damage : Int
        }


tick : Float -> Effect -> Effect
tick delta effect =
    case effect of
        Damage eff ->
            Damage
                { eff | reloading = Basics.max 0 (eff.reloading - delta) }
