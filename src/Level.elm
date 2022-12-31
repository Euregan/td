module Level exposing (..)


type alias Level =
    { width : Int
    , height : Int
    , start : ( Int, Int )
    }


init : ( Int, Int ) -> Level
init ( width, height ) =
    Level width height ( 1, 1 )
