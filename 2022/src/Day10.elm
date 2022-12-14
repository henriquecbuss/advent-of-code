module Day10 exposing (finalInput, sampleInput, solution1, solution2)

import Array exposing (Array)
import Dict exposing (Dict)
import List.Extra
import Parser exposing ((|.), (|=), Parser)


solution1 : String -> String
solution1 input =
    input
        |> String.trim
        |> Parser.run inputParser
        |> Result.map
            (List.foldl runInstruction initialState
                >> (\state ->
                        let
                            interestingCycles =
                                [ 20, 60, 100, 140, 180, 220 ]
                                    |> List.map (\cycle -> ( cycle, 0 ))
                                    |> Dict.fromList
                        in
                        Dict.merge
                            (\_ _ -> identity)
                            (\cycle { xValue } _ result -> cycle * xValue + result)
                            (\_ _ -> identity)
                            state.history
                            interestingCycles
                            0
                   )
            )
        |> Debug.toString


solution2 : String -> String
solution2 input =
    input
        |> String.trim
        |> Parser.run inputParser
        |> Result.map
            (List.foldl runInstruction2 initialState
                >> (\state ->
                        Dict.foldl
                            (\cycle { xValue } crtRows ->
                                let
                                    absoluteCycle =
                                        modBy 40 cycle

                                    charToDraw =
                                        if abs (absoluteCycle - xValue) <= 1 then
                                            '#'

                                        else
                                            '.'
                                in
                                case Array.get (Array.length crtRows - 1) crtRows of
                                    Nothing ->
                                        Array.push (Array.fromList [ charToDraw ]) crtRows

                                    Just arr ->
                                        if Array.length arr == 40 then
                                            Array.push (Array.fromList [ charToDraw ]) crtRows

                                        else
                                            Array.set (Array.length crtRows - 1) (Array.push charToDraw arr) crtRows
                            )
                            Array.empty
                            state.history
                   )
                >> crtRowsToString
            )
        |> Result.withDefault ""



-- |> Debug.toString


crtRowsToString : Array (Array Char) -> String
crtRowsToString rows =
    Array.map (Array.toList >> String.fromList) rows
        |> Array.toList
        |> String.join "\n"


runInstruction :
    Instruction
    -> State
    -> State
runInstruction instruction state =
    case instruction of
        NoOp ->
            { state
                | currentCycle = state.currentCycle + 1
                , history = Dict.insert (state.currentCycle + 1) { xValue = state.xValue } state.history
            }

        AddX x ->
            { state
                | currentCycle = state.currentCycle + 2
                , xValue = state.xValue + x
                , history =
                    state.history
                        |> Dict.insert (state.currentCycle + 1) { xValue = state.xValue }
                        |> Dict.insert (state.currentCycle + 2) { xValue = state.xValue }
            }


runInstruction2 :
    Instruction
    -> State
    -> State
runInstruction2 instruction state =
    case instruction of
        NoOp ->
            { state
                | currentCycle = state.currentCycle + 1
                , history = Dict.insert (state.currentCycle + 1) { xValue = state.xValue } state.history
            }

        AddX x ->
            { state
                | currentCycle = state.currentCycle + 2
                , xValue = state.xValue + x
                , history =
                    state.history
                        |> Dict.insert (state.currentCycle + 1) { xValue = state.xValue }
                        |> Dict.insert (state.currentCycle + 2) { xValue = state.xValue + x }
            }


getEvery : Int -> List a -> List a
getEvery interval list =
    case List.drop interval list of
        head :: tail ->
            head :: getEvery interval tail

        [] ->
            []


type alias State =
    { currentCycle : Int
    , xValue : Int
    , history : Dict Int { xValue : Int }
    }


initialState : State
initialState =
    { currentCycle = 0
    , xValue = 1
    , history = Dict.fromList [ ( 0, { xValue = 1 } ) ]
    }


type Instruction
    = NoOp
    | AddX Int


inputParser : Parser (List Instruction)
inputParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.succeed ()
        , item = instructionParser
        , trailing = Parser.Forbidden
        }
        |. Parser.end


instructionParser : Parser Instruction
instructionParser =
    Parser.oneOf
        [ Parser.succeed NoOp
            |. Parser.token "noop"
        , Parser.succeed AddX
            |. Parser.token "addx"
            |. Parser.spaces
            |= signedInt
        ]


signedInt : Parser Int
signedInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


sampleInput : String
sampleInput =
    """
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"""


finalInput : String
finalInput =
    """
noop
noop
addx 5
noop
noop
addx 1
addx 3
addx 2
addx 4
addx 3
noop
addx 2
addx 1
noop
noop
addx 4
noop
addx 1
addx 2
addx 5
addx 3
noop
addx -1
addx -37
addx 37
addx -34
addx 7
noop
addx -2
addx 2
noop
noop
noop
addx 5
addx 2
noop
addx 3
addx 15
addx -8
addx -9
addx 21
addx -9
addx 5
addx 2
addx 3
addx -2
addx -38
noop
addx 3
addx 37
addx -33
addx 5
noop
noop
addx 5
noop
noop
addx 5
noop
addx -1
addx 1
addx 5
noop
noop
addx 5
noop
noop
noop
addx 1
addx 2
noop
addx 3
addx -36
noop
noop
noop
addx 6
addx 21
addx -17
addx 18
addx -8
addx -7
addx 2
addx 5
addx -8
addx 13
addx -2
addx 7
noop
addx -2
addx 5
addx 2
addx 1
noop
addx -38
addx 4
addx 3
noop
addx 34
addx -29
addx -2
addx 10
addx -3
addx 2
addx 3
noop
addx -22
addx 2
addx 23
addx 7
noop
noop
addx 3
noop
addx 2
addx -18
addx 19
addx -38
addx 5
addx 2
noop
addx 1
addx 4
addx 1
noop
noop
addx 2
addx 5
addx 2
noop
addx 1
noop
addx 2
addx 8
addx -1
addx -30
addx 31
addx 2
addx 5
addx -35
noop
"""
