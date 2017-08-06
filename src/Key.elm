module Key exposing (Key(..), fromCode)


type Key
    = P
    | ArrowRight
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        80 ->
            P

        39 ->
            ArrowRight

        _ ->
            Unknown
