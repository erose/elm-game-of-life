module Main exposing (..)

import Html exposing (Html)
import Html.Attributes
import Keyboard
import Svg exposing (Svg, svg, text, rect)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (..)
import Char
import Dict exposing (Dict)
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Point =
    ( Int, Int )


type alias Board =
    Dict Point Bool


type alias Model =
    { board : Board, paused : Bool }


cellsOnASide : Int
cellsOnASide =
    50


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard cellsOnASide, paused = True }, Cmd.none )


initialBoard : Int -> Board
initialBoard sideLength =
    let
        width =
            sideLength

        height =
            sideLength
    in
        Dict.fromList <|
            List.map
                (\point -> ( point, False ))
                (cartesianProduct (List.range 0 width) (List.range 0 height))


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs



-- UPDATE


type Msg
    = ToggleCell Point
    | TogglePause
    | NoOp
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                ToggleCell point ->
                    { model | board = Dict.update point toggleCell model.board }

                Tick time ->
                    let
                        { paused, board } =
                            model
                    in
                        { model
                            | board =
                                if paused then
                                    board
                                else
                                    stepSimulation board
                        }

                TogglePause ->
                    { model | paused = not model.paused }

                NoOp ->
                    model
    in
        ( newModel, Cmd.none )


toggleCell : Maybe Bool -> Maybe Bool
toggleCell maybeIsAlive =
    case maybeIsAlive of
        Just isAlive ->
            Just (not isAlive)

        Nothing ->
            -- This case should never happen because you can't click on a cell that's not present.
            Nothing


stepSimulation : Board -> Board
stepSimulation board =
    Dict.map (determineLifeOrDeath board) board


determineLifeOrDeath : Board -> Point -> Bool -> Bool
determineLifeOrDeath board point isAlive =
    case (numberOfLivingNeighbors point board) of
        0 ->
            False

        1 ->
            False

        2 ->
            isAlive

        3 ->
            True

        _ ->
            False


numberOfLivingNeighbors : Point -> Board -> Int
numberOfLivingNeighbors point board =
    let
        isNeighborLiving neighborPoint =
            case (Dict.get neighborPoint board) of
                Just isAlive ->
                    isAlive

                Nothing ->
                    False

        livingNeighbors =
            List.filter isNeighborLiving <| neighborhood point
    in
        List.length livingNeighbors


neighborhood : Point -> List Point
neighborhood ( cellX, cellY ) =
    let
        ( x, y ) =
            ( cellX, cellY )
    in
        [ -- Top row.
          ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )

        -- Middle row. Note that (x, y) isn't a neighbor of itself.
        , ( x - 1, y )
        , ( x + 1, y )

        -- Bottom row.
        , ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        ]



-- VIEW


cellSizeInPixels : Int
cellSizeInPixels =
    10


view : Model -> Html Msg
view model =
    let
        boardSizeInPixels =
            toString <| cellsOnASide * cellSizeInPixels
    in
        Html.div []
            [ svg
                [ width boardSizeInPixels
                , height boardSizeInPixels
                , Html.Attributes.style <|
                    [ ( "border", "1px solid black" ) ]
                        ++ if model.paused then
                            [ ( "backgroundColor", "lightBlue" ) ]
                           else
                            []
                ]
                (renderBoard model.board)
            , Html.div [] [ text helpText ]
            ]


helpText : String
helpText =
    """
Click to set up your cells, then press 'p' to unpause.
"""


renderBoard : Board -> List (Svg Msg)
renderBoard board =
    List.map renderCell <| Dict.toList board


renderCell : ( Point, Bool ) -> Svg Msg
renderCell ( ( cellX, cellY ), isAlive ) =
    rect
        [ x <| toScreenCoordinate cellX
        , y <| toScreenCoordinate cellY
        , width <| toString cellSizeInPixels
        , height <| toString cellSizeInPixels
        , onClick <| ToggleCell ( cellX, cellY )
        , fillOpacity <|
            if isAlive then
                "1.0"
            else
                "0.0"
        ]
        []


toScreenCoordinate : Int -> String
toScreenCoordinate n =
    toString <| n * cellSizeInPixels



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (100 * Time.millisecond) Tick
        , Keyboard.downs
            (\keyCode ->
                case (Char.fromCode keyCode) of
                    'P' ->
                        TogglePause

                    _ ->
                        NoOp
            )
        ]
