module Day09 exposing (finalInput, sampleInput, sampleInput2, solution1, solution2)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Time



-- VISUALIZATION


type alias Model =
    { playingStatus : PlayingStatus
    , state : State2
    , statesStack : List ( State2, Direction )
    , directionsStack : List Direction
    }


type PlayingStatus
    = Playing
    | Paused


type Msg
    = Ticked
    | WentBack
    | ToggledPlayingStatus


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { playingStatus = Paused
      , state = initialState2
      , statesStack = []
      , directionsStack =
            finalInput
                |> String.trim
                |> Parser.run inputParser
                |> Result.withDefault []
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            []
            [ case model.playingStatus of
                Playing ->
                    Html.button [ onClick ToggledPlayingStatus ] [ Html.text "Pause" ]

                Paused ->
                    Html.button [ onClick ToggledPlayingStatus ] [ Html.text "Play" ]
            , Html.button
                [ onClick WentBack
                , disabled (model.playingStatus == Playing || List.isEmpty model.statesStack)
                ]
                [ Html.text "Go back" ]
            , Html.button
                [ onClick Ticked
                , disabled (model.playingStatus == Playing || List.isEmpty model.directionsStack)
                ]
                [ Html.text "Go forward" ]
            ]
        , Html.p []
            [ Html.text "Tail visited "
            , Html.text (String.fromInt (Set.size model.state.tailVisitedCells))
            , Html.text " positions: "
            ]
        , Html.p []
            [ Html.text
                (model.state.tailVisitedCells
                    |> Set.toList
                    |> List.map (\( x, y ) -> "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")")
                    |> String.join "   "
                )
            ]
        , Html.div
            [ style "position" "relative"
            ]
            ((List.range -30 30
                |> List.concatMap
                    (\y ->
                        List.range -30 30
                            |> List.map
                                (\x ->
                                    Html.div
                                        [ style "position" "absolute"
                                        , style "left" (String.fromInt (400 + x * 10) ++ "px")
                                        , style "top" (String.fromInt (300 + y * 10) ++ "px")
                                        , style "width" "10px"
                                        , style "height" "10px"
                                        , style "border" "solid 1px lightgray"
                                        ]
                                        []
                                )
                    )
             )
                ++ viewRope model.state.tail
            )
        ]


viewRope : Rope -> List (Html Msg)
viewRope rope =
    case rope of
        Head position ->
            [ viewPosition position ]

        Knot position next ->
            viewPosition position :: viewRope next


viewPosition : Position -> Html Msg
viewPosition position =
    Html.div
        [ style "position" "absolute"
        , style "left" (String.fromInt (400 + position.x * 10) ++ "px")
        , style "top" (String.fromInt (300 + position.y * 10) ++ "px")
        , style "width" "10px"
        , style "height" "10px"
        , style "background-color" "black"
        , style "border" "solid 1px gray"
        ]
        [ Html.text "*" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ticked ->
            case model.directionsStack of
                first :: others ->
                    ( { model
                        | state = updateState2 first model.state
                        , directionsStack = others
                        , statesStack = ( model.state, first ) :: model.statesStack
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        WentBack ->
            case model.statesStack of
                ( state, direction ) :: others ->
                    ( { model
                        | state = state
                        , statesStack = others
                        , directionsStack = direction :: model.directionsStack
                      }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        ToggledPlayingStatus ->
            ( { model
                | playingStatus =
                    case model.playingStatus of
                        Paused ->
                            Playing

                        Playing ->
                            Paused
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.playingStatus of
        Playing ->
            Time.every 100 (\_ -> Ticked)

        Paused ->
            Sub.none



-- SOLUTION 1


solution1 : String -> String
solution1 input =
    input
        |> String.trim
        |> Parser.run inputParser
        |> Result.map
            (List.foldl (\direction -> moveHead direction >> moveTail) initialState
                >> .tailVisitedCells
                >> Set.size
            )
        |> Debug.toString


solution2 : String -> String
solution2 input =
    input
        |> String.trim
        |> Parser.run inputParser
        |> Result.map
            (List.foldl updateState2 initialState2
                >> .tailVisitedCells
                >> Set.size
            )
        |> Debug.toString


updateState2 : Direction -> State2 -> State2
updateState2 direction state =
    let
        newTail =
            move direction state.tail
    in
    { tail = newTail
    , tailVisitedCells =
        Set.insert
            (getPosition newTail |> positionToTuple)
            state.tailVisitedCells
    }


applyDirection : Direction -> Position -> Position
applyDirection direction position =
    case direction of
        Right ->
            { position | x = position.x + 1 }

        Up ->
            { position | y = position.y + 1 }

        Left ->
            { position | x = position.x - 1 }

        Down ->
            { position | y = position.y - 1 }


moveHead : Direction -> State -> State
moveHead direction ({ head } as state) =
    { state | head = applyDirection direction head }


moveTail : State -> State
moveTail state =
    case findMovement state.head state.tail of
        Just position ->
            moveTailTo position state

        Nothing ->
            state


findMovement : Position -> Position -> Maybe Position
findMovement head tail =
    if head.x == tail.x && abs (head.y - tail.y) > 1 then
        -- Tail needs to go up or down
        if head.y > tail.y then
            Just { tail | y = tail.y + 1 }

        else
            Just { tail | y = tail.y - 1 }

    else if head.y == tail.y && abs (head.x - tail.x) > 1 then
        -- Tail needs to go left or right
        if head.x > tail.x then
            Just { tail | x = tail.x + 1 }

        else
            Just { tail | x = tail.x - 1 }

    else if abs (head.x - tail.x) == 1 && head.y - tail.y > 1 then
        Just { x = head.x, y = head.y - 1 }

    else if abs (head.x - tail.x) == 1 && head.y - tail.y < -1 then
        Just { x = head.x, y = head.y + 1 }

    else if abs (head.y - tail.y) == 1 && head.x - tail.x > 1 then
        Just { x = head.x - 1, y = head.y }

    else if abs (head.y - tail.y) == 1 && head.x - tail.x < -1 then
        Just { x = head.x + 1, y = head.y }

    else if head.x - tail.x > 1 && head.y - tail.y > 1 then
        Just { x = head.x - 1, y = head.y - 1 }

    else if head.x - tail.x > 1 && head.y - tail.y < -1 then
        Just { x = head.x - 1, y = head.y + 1 }

    else if head.x - tail.x < -1 && head.y - tail.y > 1 then
        Just { x = head.x + 1, y = head.y - 1 }

    else if head.x - tail.x < -1 && head.y - tail.y < -1 then
        Just { x = head.x + 1, y = head.y + 1 }

    else
        Nothing


moveTailTo : Position -> State -> State
moveTailTo position state =
    { state
        | tail = position
        , tailVisitedCells = Set.insert ( position.x, position.y ) state.tailVisitedCells
    }


type alias Position =
    { x : Int, y : Int }


initialState : State
initialState =
    { head = { x = 0, y = 0 }
    , tail = { x = 0, y = 0 }
    , tailVisitedCells = Set.fromList [ ( 0, 0 ) ]
    }


type alias State =
    { head : Position
    , tail : Position
    , tailVisitedCells : Set ( Int, Int )
    }


type Direction
    = Right
    | Up
    | Left
    | Down


inputParser : Parser (List Direction)
inputParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = Parser.succeed ()
        , item = lineParser
        , trailing = Parser.Forbidden
        }
        |. Parser.end
        |> Parser.map List.concat


lineParser : Parser (List Direction)
lineParser =
    Parser.succeed (\direction amount -> List.repeat amount direction)
        |= Parser.oneOf
            [ Parser.succeed Right
                |. Parser.token "R"
            , Parser.succeed Up
                |. Parser.token "U"
            , Parser.succeed Left
                |. Parser.token "L"
            , Parser.succeed Down
                |. Parser.token "D"
            ]
        |. Parser.spaces
        |= Parser.int



-- SOLUTION 2


type alias State2 =
    { tail : Rope
    , tailVisitedCells : Set ( Int, Int )
    }


initialState2 : State2
initialState2 =
    { tail = initialRope
    , tailVisitedCells = Set.fromList [ ( 0, 0 ) ]
    }


initialRope : Rope
initialRope =
    List.range 0 8
        |> List.foldl (\_ -> Knot { x = 0, y = 0 }) (Head { x = 0, y = 0 })


positionToTuple : Position -> ( Int, Int )
positionToTuple position =
    ( position.x, position.y )


type Rope
    = Head Position
    | Knot Position Rope


move : Direction -> Rope -> Rope
move direction rope =
    case rope of
        Head pos ->
            Head (applyDirection direction pos)

        Knot pos nextRope ->
            let
                newNextRope =
                    move direction nextRope
            in
            case findMovement (getPosition newNextRope) pos of
                Just newPos ->
                    Knot newPos newNextRope

                Nothing ->
                    Knot pos newNextRope


getPosition : Rope -> Position
getPosition rope =
    case rope of
        Head pos ->
            pos

        Knot pos _ ->
            pos


sampleInput : String
sampleInput =
    """
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""


sampleInput2 : String
sampleInput2 =
    """
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""


finalInput : String
finalInput =
    """
D 1
U 2
R 2
U 1
R 1
U 2
L 2
D 1
L 2
R 1
L 1
R 1
U 2
D 2
L 2
R 2
L 2
R 1
L 1
D 1
U 1
R 1
U 2
D 1
U 1
D 1
R 2
D 1
U 1
R 1
L 2
R 2
U 2
R 2
L 2
R 2
U 2
D 2
R 1
U 2
D 2
U 2
D 1
L 1
D 1
L 1
R 2
D 2
R 2
D 1
R 2
L 1
R 1
L 2
D 2
U 1
D 1
R 2
L 2
D 2
L 2
D 2
L 1
R 2
L 1
D 1
L 2
D 1
U 1
R 1
U 1
R 2
D 2
R 1
U 2
D 1
L 2
D 2
L 1
D 1
L 1
D 1
R 1
L 1
U 1
D 2
U 1
D 2
R 1
D 2
L 2
U 1
L 1
R 1
D 1
U 1
D 1
R 1
U 2
L 1
D 1
U 2
R 2
D 2
R 1
D 1
R 1
L 1
R 2
U 1
D 1
L 1
U 3
L 2
U 2
L 2
D 2
L 1
R 3
D 1
U 1
L 3
D 3
U 1
L 2
U 1
R 2
U 3
D 2
L 3
U 2
R 2
D 2
U 1
R 1
L 3
D 2
L 3
D 1
U 3
R 3
U 3
D 3
U 3
L 3
U 1
D 1
U 1
D 3
R 3
D 3
U 3
R 3
L 3
R 2
U 3
D 3
L 1
R 2
D 2
U 1
R 1
L 2
D 3
R 3
D 1
U 1
D 1
U 2
D 1
R 1
D 2
R 2
L 1
D 2
L 1
R 2
L 3
R 3
L 1
U 3
R 3
U 3
D 1
L 3
R 1
L 1
R 2
L 1
U 2
R 1
D 2
R 3
L 3
R 2
U 3
R 3
L 1
R 1
L 1
U 1
L 2
R 2
L 3
D 2
L 2
R 3
D 3
U 1
L 2
D 3
U 3
D 3
R 1
U 1
L 2
U 1
L 1
R 1
U 3
L 2
R 1
D 4
R 3
D 3
L 3
D 4
R 4
L 3
R 2
D 2
R 4
U 2
D 3
L 1
R 2
U 4
R 3
D 2
U 1
D 4
R 1
D 3
U 3
L 2
R 2
L 3
R 3
L 4
D 3
L 2
U 3
L 1
R 2
D 4
R 3
U 3
L 2
U 3
D 2
L 4
D 4
L 4
U 4
R 1
D 1
R 3
U 3
R 3
D 3
L 1
U 1
R 3
L 2
R 2
D 2
R 4
U 4
L 4
D 3
L 2
R 4
L 2
D 1
L 2
R 4
U 2
L 2
U 3
L 4
R 2
L 4
D 2
L 1
D 4
L 3
U 2
D 1
R 4
U 2
D 1
R 4
D 4
U 3
R 3
U 4
L 1
D 3
U 1
D 3
R 2
U 1
R 3
L 4
R 1
L 4
D 3
R 4
U 2
L 1
D 4
L 2
R 4
L 2
R 3
L 3
U 2
R 3
L 2
R 1
D 4
L 3
D 3
L 5
D 1
R 3
D 4
L 4
R 2
D 5
R 2
L 2
D 5
U 2
D 1
U 2
D 3
L 2
D 1
L 3
U 1
D 3
R 2
U 4
R 5
D 5
U 3
R 1
D 5
U 1
D 4
R 1
U 4
D 4
R 4
U 5
D 4
R 1
L 1
D 5
L 1
D 4
U 1
D 2
U 1
D 1
R 1
D 2
R 2
D 1
L 5
R 3
L 4
D 3
R 5
D 4
U 2
R 2
U 2
D 2
U 5
L 4
D 5
L 5
R 3
D 2
U 4
D 2
L 3
R 2
L 1
U 2
R 4
L 5
R 4
D 1
U 5
D 4
L 5
R 2
U 4
D 3
L 3
D 3
L 1
U 5
L 4
R 1
U 5
D 4
R 2
L 1
D 2
R 3
L 2
R 5
D 5
R 2
L 1
U 3
D 2
L 1
U 1
R 4
U 1
D 1
U 1
R 2
U 3
R 4
L 4
R 1
U 3
R 3
D 1
L 3
R 4
L 1
U 5
D 6
U 2
L 4
D 2
U 1
D 4
R 5
L 2
R 6
L 6
U 1
D 4
L 3
D 5
U 4
L 4
R 2
D 2
L 3
D 1
U 6
L 6
R 2
D 2
U 3
L 6
R 1
U 5
R 5
U 5
D 1
L 2
U 1
R 1
D 5
L 6
R 1
D 4
R 3
L 3
D 2
U 5
D 6
U 3
D 6
U 4
L 2
R 1
U 5
D 2
L 1
D 4
U 1
R 2
L 2
R 4
D 1
L 6
D 5
L 6
R 6
L 3
U 5
R 4
D 4
L 3
D 2
L 1
R 4
U 3
R 2
U 2
R 5
D 1
U 5
D 4
R 4
L 5
D 5
U 3
R 1
U 4
L 5
U 1
D 2
U 4
D 4
L 1
U 2
L 3
U 5
R 1
L 5
U 1
R 4
D 2
L 6
U 6
R 6
U 5
R 6
U 6
L 5
U 1
L 5
D 3
R 1
D 4
L 3
D 7
U 1
D 4
R 2
U 4
D 4
U 7
D 3
L 3
D 3
R 4
D 7
R 4
D 5
U 3
L 5
R 5
D 5
L 2
R 3
L 2
R 4
U 2
L 4
U 7
L 2
U 6
D 3
U 6
R 2
D 2
U 6
L 2
R 4
U 5
D 2
R 4
L 2
D 2
R 1
D 3
R 1
D 3
U 4
D 2
L 4
D 1
L 5
U 1
R 2
D 3
U 4
D 4
L 5
R 2
D 2
U 1
D 7
L 7
R 2
L 6
D 4
R 7
L 3
U 2
D 2
U 4
L 5
D 4
U 4
R 5
L 5
D 2
R 5
L 7
U 1
D 7
L 3
R 4
L 5
D 5
L 2
R 5
L 3
R 2
L 4
R 1
D 3
L 5
R 2
D 4
U 3
R 3
L 5
D 3
R 1
D 5
R 5
L 1
D 6
R 5
D 4
U 2
L 4
U 1
D 3
L 2
D 7
R 4
L 1
U 6
R 8
L 2
U 2
R 1
U 7
D 6
L 7
R 3
U 5
R 4
L 8
D 2
R 4
U 8
L 6
R 1
L 7
D 5
L 3
R 6
U 1
D 7
L 2
U 7
R 1
U 6
L 3
R 8
D 5
L 4
R 4
L 6
D 6
R 3
L 2
U 5
R 3
D 1
R 7
L 1
U 8
D 2
U 3
D 1
L 2
U 6
L 1
U 4
R 6
U 7
R 1
D 1
U 8
R 1
D 7
L 1
U 4
D 1
U 2
R 8
U 1
D 6
R 1
U 1
R 5
L 7
R 3
D 3
U 8
L 6
D 7
U 8
L 4
R 1
L 7
U 4
D 3
L 3
R 6
D 3
U 8
L 5
D 8
L 1
U 7
R 6
L 5
U 8
R 2
L 8
R 7
L 8
U 7
D 7
U 3
D 4
U 8
D 5
U 3
L 4
D 4
U 8
R 7
U 7
L 7
R 4
L 1
D 8
R 9
D 4
R 8
L 5
R 9
U 5
D 1
L 5
D 7
U 3
R 5
U 1
R 3
U 2
D 9
U 5
R 3
U 4
R 3
D 4
U 7
D 9
L 7
D 3
U 1
R 3
U 4
L 3
R 8
L 3
U 8
R 6
L 6
D 7
L 7
D 4
L 3
U 5
L 2
R 5
D 3
U 6
L 9
R 9
D 9
U 9
R 4
D 8
R 8
D 5
L 7
U 2
R 4
L 8
R 4
L 2
R 3
L 7
R 1
U 1
L 5
D 5
R 9
D 8
U 7
L 3
R 5
D 1
U 1
R 5
D 6
U 9
D 8
R 4
D 7
L 1
U 5
D 8
L 4
D 4
L 7
D 8
R 5
U 9
D 2
L 1
R 2
U 8
D 4
L 6
D 1
L 5
D 6
R 9
U 1
L 9
U 4
D 6
R 8
L 5
R 1
L 2
R 7
D 8
L 4
R 3
U 2
L 7
U 2
D 1
R 8
U 8
L 1
R 1
D 2
U 3
L 5
D 6
L 3
U 6
R 2
L 5
U 6
L 5
D 3
L 6
D 2
L 4
U 10
R 4
D 7
U 4
D 4
L 5
U 3
L 9
R 7
D 1
L 4
U 4
D 5
R 5
D 4
U 1
D 3
R 1
L 6
U 1
D 1
L 4
U 5
D 10
R 4
U 5
R 1
U 2
R 4
D 6
L 7
U 1
R 2
D 1
L 7
D 4
U 9
L 6
D 8
R 3
U 7
R 8
U 2
D 5
R 3
U 10
D 3
R 3
U 1
R 8
D 10
R 3
D 1
U 10
L 5
U 8
D 6
L 9
D 5
U 1
R 9
U 3
D 3
R 8
L 9
D 8
U 3
R 2
D 3
L 7
D 3
R 10
L 9
U 2
R 8
U 8
R 5
U 3
L 9
U 1
D 7
L 7
R 5
L 8
U 8
L 9
R 10
U 1
R 5
L 5
D 7
L 8
D 5
U 4
L 4
U 6
D 4
L 8
U 8
L 10
D 1
L 2
D 3
U 8
L 7
R 7
U 6
D 4
U 8
L 2
R 6
L 7
D 11
R 4
D 9
L 10
U 3
L 9
U 10
L 11
U 11
D 2
U 4
D 5
L 11
D 4
L 6
U 2
D 8
R 4
U 4
D 5
U 8
L 3
U 2
R 7
U 10
R 5
L 3
R 11
U 2
D 8
U 3
R 10
U 10
L 3
R 7
L 8
D 5
U 9
D 9
R 3
U 4
L 6
D 9
R 10
L 2
R 8
U 11
L 2
R 1
U 11
R 2
D 3
U 9
R 3
L 2
R 1
L 6
R 10
L 2
U 6
R 4
L 9
U 11
L 1
R 11
U 8
L 4
R 11
L 6
D 5
L 11
U 3
D 5
R 5
U 2
L 3
R 4
L 9
R 7
U 11
R 7
U 7
D 10
R 11
D 10
L 3
D 5
U 2
L 1
U 2
R 2
U 8
R 1
L 10
R 10
U 5
D 4
U 4
R 4
L 8
R 6
U 9
R 5
L 3
U 9
L 3
D 4
R 10
D 11
L 12
R 1
U 11
L 10
R 11
D 10
R 10
L 4
U 11
D 2
R 8
L 6
U 7
D 7
L 12
U 1
L 3
D 8
R 6
L 7
R 11
L 2
R 6
U 2
R 2
L 2
U 5
R 12
U 12
D 12
U 1
D 6
L 5
U 3
D 3
L 4
R 10
L 5
U 2
D 8
R 4
D 5
L 7
U 2
R 10
D 12
R 9
U 11
D 10
R 4
U 2
D 1
U 10
D 8
R 2
L 9
D 12
U 12
D 2
U 8
R 5
L 8
D 5
U 9
D 8
U 7
R 3
L 10
U 11
L 5
U 12
L 2
U 9
D 7
R 12
U 9
R 2
L 3
U 10
D 9
L 7
U 5
R 3
D 3
L 11
D 1
R 5
L 1
U 5
L 6
D 8
L 2
D 5
U 8
D 9
R 8
L 7
D 6
L 6
D 1
U 5
L 4
R 6
U 9
L 3
U 3
D 7
L 1
D 4
L 9
R 9
U 6
D 8
U 12
L 5
D 4
R 7
L 2
D 4
U 7
R 7
U 2
R 13
D 5
L 1
R 5
D 6
U 2
L 1
U 5
D 2
R 9
L 8
U 9
D 9
U 12
R 10
U 9
L 13
R 5
D 10
L 9
U 4
D 12
U 12
R 13
D 12
U 7
R 12
D 8
L 8
U 2
D 10
L 2
D 5
U 3
R 11
D 4
R 4
L 1
R 1
D 9
U 6
R 10
U 12
L 10
R 13
D 7
R 12
L 5
R 10
D 4
R 11
L 7
R 7
U 7
D 7
U 11
L 3
R 9
L 12
D 11
R 5
D 5
R 7
D 7
U 3
L 11
D 11
L 10
U 7
L 13
R 6
L 10
U 10
D 10
L 4
R 5
U 5
D 2
L 6
D 8
U 1
R 12
D 11
U 3
L 9
R 10
U 8
R 10
D 2
R 3
U 9
D 7
L 6
R 9
D 2
U 11
R 3
D 1
R 4
L 11
U 13
R 12
U 12
R 8
L 13
U 7
D 10
R 11
L 2
U 9
L 3
R 5
L 4
R 11
D 5
R 1
L 11
U 10
R 4
L 1
U 2
D 8
L 7
U 1
L 4
D 1
R 1
L 11
U 8
R 11
U 4
R 10
L 6
D 7
U 9
L 14
U 1
R 10
U 12
R 1
L 6
D 9
U 10
L 10
D 5
R 14
U 3
R 13
U 3
L 13
U 8
L 10
U 7
R 12
U 11
R 6
U 6
D 6
R 2
L 7
R 8
U 8
D 3
R 8
L 12
U 11
D 9
U 6
R 8
D 8
U 9
R 12
L 11
U 10
L 14
R 9
D 8
U 14
L 9
D 11
U 2
R 2
U 4
R 7
U 11
L 8
D 12
L 11
U 13
L 1
D 8
U 8
L 10
U 12
R 11
U 3
D 12
L 1
D 8
L 4
D 3
L 2
U 8
D 9
L 1
U 9
D 15
U 5
L 9
D 2
U 4
L 2
U 15
L 8
R 2
U 1
L 3
D 10
L 15
U 2
R 14
D 1
U 13
D 4
R 8
D 6
R 5
D 13
L 5
R 6
U 14
L 15
D 15
U 12
L 1
D 3
L 6
D 2
R 4
D 10
U 12
R 13
L 3
R 15
D 13
U 15
D 14
U 8
D 10
U 5
D 11
R 15
D 7
R 1
D 2
L 5
U 11
L 14
U 9
D 13
R 12
U 10
R 11
D 9
R 6
D 7
L 7
U 14
D 12
L 6
D 15
U 11
R 1
U 3
L 11
R 3
L 9
R 9
L 11
R 12
U 6
R 13
D 13
L 9
U 3
D 6
L 4
D 3
U 15
D 13
U 9
D 1
U 13
D 12
L 4
U 6
R 11
D 2
R 7
U 9
R 11
U 3
L 7
U 4
R 2
D 14
U 3
D 14
R 13
L 4
R 1
U 12
D 10
L 8
D 2
L 15
U 1
D 4
U 16
R 15
D 1
R 8
U 14
L 12
U 2
R 1
D 16
L 10
R 11
D 10
L 15
R 7
U 10
L 3
U 11
D 1
L 5
D 3
U 8
R 3
U 5
R 2
D 11
L 2
D 7
L 2
U 3
L 12
D 13
L 12
U 7
R 11
L 4
R 11
L 8
U 16
L 1
R 11
L 4
U 15
L 16
R 15
U 6
R 4
U 3
D 16
R 9
U 8
R 5
U 6
L 2
R 2
D 10
R 7
D 13
U 13
R 14
D 3
L 15
R 3
D 11
L 4
U 12
D 1
U 8
R 8
L 4
D 6
L 15
U 7
D 15
L 11
U 14
D 8
U 15
L 12
R 6
U 13
R 5
U 2
L 1
R 13
D 10
R 7
L 9
U 14
R 1
D 13
L 11
U 9
L 13
R 5
D 14
R 16
U 5
D 9
R 2
D 5
R 8
L 8
U 16
D 12
R 3
U 3
D 5
U 12
D 14
L 9
U 12
R 6
L 9
U 3
D 16
U 15
R 6
D 11
R 10
D 3
U 16
L 11
R 2
L 2
R 9
D 12
L 8
D 8
R 13
L 5
D 17
L 7
D 10
U 13
L 11
U 17
L 13
R 2
D 6
R 14
D 14
L 15
R 8
D 4
U 7
L 6
R 6
U 7
D 12
R 9
L 8
D 15
L 14
R 1
U 8
L 13
D 16
R 2
U 2
L 2
D 13
U 8
D 6
U 10
L 9
R 2
L 7
U 5
R 6
U 12
D 3
R 1
U 4
R 17
U 11
L 3
U 13
L 12
U 6
R 5
D 12
R 4
U 4
R 3
D 11
U 7
L 10
D 12
U 12
D 9
R 4
L 2
R 9
L 13
D 16
L 14
D 5
L 8
D 6
L 13
U 1
R 7
U 5
R 2
U 11
L 10
D 14
R 11
L 5
D 16
U 9
D 3
U 11
D 1
U 5
D 3
U 8
L 1
D 1
U 5
L 17
D 9
L 14
R 8
D 15
R 10
U 4
R 1
U 3
R 5
L 13
D 6
L 15
U 10
L 5
R 14
L 1
R 6
D 10
L 13
D 3
L 1
U 10
D 6
R 16
D 6
R 4
D 9
U 9
D 4
R 1
D 10
R 7
D 14
R 10
D 14
U 14
D 15
R 11
L 16
D 18
R 6
U 8
D 18
R 18
D 14
U 9
R 18
U 14
L 18
U 1
R 14
D 1
U 6
R 3
L 13
D 4
L 9
D 2
U 5
D 6
U 9
L 13
U 12
R 11
D 17
U 17
L 8
U 15
R 16
L 9
D 2
R 16
D 2
L 9
D 9
R 15
D 10
L 12
R 14
L 4
R 17
D 15
U 16
R 7
D 4
U 4
L 7
D 16
R 11
D 2
U 2
L 7
U 15
L 5
U 14
L 7
U 15
D 9
L 14
D 13
U 11
R 10
U 15
L 5
U 16
D 13
U 15
L 18
R 1
D 17
U 18
R 19
L 4
R 15
U 12
D 19
R 5
D 18
L 14
U 5
L 4
R 6
U 15
L 7
R 11
L 15
U 12
D 3
R 14
U 1
L 4
R 3
U 15
L 15
R 6
L 3
U 2
D 6
L 6
D 16
L 19
U 9
L 2
R 13
U 8
R 17
D 17
L 4
R 2
L 7
R 11
D 5
L 14
D 7
R 13
D 2
L 18
U 11
R 2
D 18
U 2
D 6
L 4
U 2
L 15
R 4
D 8
R 1
U 16
L 4
R 5
U 16
D 14
U 2
D 19
L 6
D 14
U 17
L 3
R 11
L 2
R 11
U 11
D 4
U 13
L 6
U 14
D 17
U 15
D 7
R 8
U 3
R 15
D 17
U 16
R 3
D 17
L 9
U 15
R 15
L 11
R 18
D 16
R 17
D 3
L 17
D 4
L 10
U 2
R 2
D 9
L 17
R 19
"""
