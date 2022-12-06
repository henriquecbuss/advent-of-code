module Day05 exposing (finalInput, sampleInput, solution1, solution2)

import List.Extra


type alias MoveInstruction =
    { move : Int
    , from : Int
    , to : Int
    }


solution1 : String -> String
solution1 input =
    input
        |> splitIntoCratesAndInstructions
        |> Maybe.andThen
            (\{ cratesString, instructionsString } ->
                let
                    initialConfiguration : List (List Char)
                    initialConfiguration =
                        cratesString
                            |> String.lines
                            |> List.map getCratesInLine
                            |> List.Extra.transpose
                            |> List.map (List.filterMap parseValidCrate)

                    moves : List MoveInstruction
                    moves =
                        instructionsString
                            |> String.lines
                            |> List.filterMap parseInstructionLine
                in
                List.foldl
                    (\move -> Maybe.andThen (executeInstruction move))
                    (Just initialConfiguration)
                    moves
            )
        |> Maybe.map
            (\allCrates ->
                List.filterMap List.head allCrates
                    |> String.fromList
            )
        |> Maybe.withDefault "Uh oh"
        |> Debug.toString


solution2 : String -> String
solution2 input =
    input
        |> splitIntoCratesAndInstructions
        |> Maybe.andThen
            (\{ cratesString, instructionsString } ->
                let
                    initialConfiguration : List (List Char)
                    initialConfiguration =
                        cratesString
                            |> String.lines
                            |> List.map getCratesInLine
                            |> List.Extra.transpose
                            |> List.map (List.filterMap parseValidCrate)

                    moves : List MoveInstruction
                    moves =
                        instructionsString
                            |> String.lines
                            |> List.filterMap parseInstructionLine
                in
                List.foldl
                    (\move -> Maybe.andThen (executeInstructionWithCrateMover9001 move))
                    (Just initialConfiguration)
                    moves
            )
        |> Maybe.map
            (\allCrates ->
                List.filterMap List.head allCrates
                    |> String.fromList
            )
        |> Maybe.withDefault "Uh oh"
        |> Debug.toString


splitIntoCratesAndInstructions : String -> Maybe { cratesString : String, instructionsString : String }
splitIntoCratesAndInstructions input =
    case String.split "\n\n" input of
        [ cratesString, instructionsString ] ->
            Just
                { cratesString =
                    cratesString
                        |> String.lines
                        |> (\lines ->
                                lines
                                    |> List.take (List.length lines - 1)
                                    |> String.join "\n"
                           )
                , instructionsString = instructionsString
                }

        _ ->
            Nothing


getCratesInLine : String -> List Char
getCratesInLine line =
    line
        |> String.toList
        |> List.drop 1
        |> takeEvery 3


parseValidCrate : Char -> Maybe Char
parseValidCrate crate =
    case crate of
        ' ' ->
            Nothing

        _ ->
            Just crate


takeEvery : Int -> List a -> List a
takeEvery gap inputList =
    case List.head inputList of
        Nothing ->
            []

        Just head ->
            head :: takeEvery gap (List.drop (gap + 1) inputList)


parseInstructionLine : String -> Maybe MoveInstruction
parseInstructionLine line =
    case String.split " " line of
        [ "move", moveString, "from", fromString, "to", toString ] ->
            Maybe.map3
                (\move from to ->
                    { move = move
                    , from = from - 1
                    , to = to - 1
                    }
                )
                (String.toInt moveString)
                (String.toInt fromString)
                (String.toInt toString)

        _ ->
            Nothing


executeInstruction : MoveInstruction -> List (List Char) -> Maybe (List (List Char))
executeInstruction instruction state =
    case List.Extra.getAt instruction.from state of
        Just fromList ->
            let
                ( movedCrates, newFromList ) =
                    List.Extra.splitAt instruction.move fromList
            in
            List.Extra.setAt instruction.from newFromList state
                |> List.Extra.updateAt instruction.to (\existingCrates -> List.reverse movedCrates ++ existingCrates)
                |> Just

        Nothing ->
            Nothing


executeInstructionWithCrateMover9001 : MoveInstruction -> List (List Char) -> Maybe (List (List Char))
executeInstructionWithCrateMover9001 instruction state =
    case List.Extra.getAt instruction.from state of
        Just fromList ->
            let
                ( movedCrates, newFromList ) =
                    List.Extra.splitAt instruction.move fromList
            in
            List.Extra.setAt instruction.from newFromList state
                |> List.Extra.updateAt instruction.to (\existingCrates -> movedCrates ++ existingCrates)
                |> Just

        Nothing ->
            Nothing


sampleInput : String
sampleInput =
    """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""


finalInput : String
finalInput =
    """[P]     [L]         [T]            
[L]     [M] [G]     [G]     [S]    
[M]     [Q] [W]     [H] [R] [G]    
[N]     [F] [M]     [D] [V] [R] [N]
[W]     [G] [Q] [P] [J] [F] [M] [C]
[V] [H] [B] [F] [H] [M] [B] [H] [B]
[B] [Q] [D] [T] [T] [B] [N] [L] [D]
[H] [M] [N] [Z] [M] [C] [M] [P] [P]
 1   2   3   4   5   6   7   8   9 

move 8 from 3 to 2
move 1 from 9 to 5
move 5 from 4 to 7
move 6 from 1 to 4
move 8 from 6 to 8
move 8 from 4 to 5
move 4 from 9 to 5
move 4 from 7 to 9
move 7 from 7 to 2
move 4 from 5 to 2
move 11 from 8 to 3
move 3 from 9 to 7
move 11 from 2 to 8
move 13 from 8 to 4
move 11 from 5 to 6
move 8 from 2 to 4
move 1 from 5 to 4
move 1 from 3 to 2
move 2 from 2 to 1
move 2 from 8 to 5
move 3 from 7 to 5
move 1 from 4 to 7
move 9 from 6 to 7
move 1 from 6 to 5
move 1 from 1 to 4
move 3 from 1 to 9
move 15 from 4 to 3
move 2 from 4 to 1
move 1 from 1 to 9
move 3 from 4 to 5
move 1 from 4 to 1
move 1 from 7 to 2
move 1 from 6 to 3
move 5 from 7 to 1
move 19 from 3 to 9
move 7 from 1 to 2
move 24 from 9 to 7
move 23 from 7 to 1
move 1 from 4 to 6
move 3 from 7 to 3
move 1 from 6 to 1
move 6 from 2 to 1
move 21 from 1 to 9
move 5 from 3 to 8
move 2 from 2 to 5
move 10 from 9 to 5
move 1 from 2 to 1
move 5 from 1 to 3
move 6 from 3 to 4
move 1 from 2 to 8
move 3 from 5 to 2
move 4 from 9 to 3
move 13 from 5 to 9
move 2 from 7 to 2
move 3 from 4 to 7
move 1 from 7 to 8
move 5 from 1 to 3
move 1 from 7 to 5
move 1 from 8 to 1
move 2 from 2 to 7
move 19 from 9 to 2
move 5 from 2 to 3
move 7 from 5 to 9
move 1 from 1 to 9
move 5 from 9 to 2
move 4 from 9 to 3
move 20 from 3 to 9
move 1 from 3 to 9
move 3 from 7 to 3
move 16 from 2 to 3
move 12 from 3 to 4
move 2 from 2 to 5
move 1 from 2 to 4
move 2 from 4 to 1
move 4 from 8 to 1
move 15 from 9 to 3
move 2 from 5 to 3
move 3 from 2 to 8
move 5 from 8 to 5
move 7 from 3 to 4
move 2 from 9 to 6
move 15 from 3 to 1
move 3 from 1 to 8
move 3 from 9 to 5
move 9 from 4 to 1
move 3 from 3 to 5
move 2 from 6 to 5
move 9 from 1 to 3
move 1 from 9 to 4
move 1 from 5 to 2
move 3 from 8 to 5
move 10 from 1 to 6
move 12 from 4 to 8
move 1 from 2 to 7
move 2 from 5 to 6
move 1 from 1 to 4
move 7 from 3 to 6
move 1 from 7 to 2
move 2 from 4 to 9
move 3 from 1 to 7
move 1 from 9 to 8
move 1 from 2 to 3
move 3 from 1 to 7
move 5 from 8 to 2
move 5 from 7 to 1
move 9 from 6 to 8
move 6 from 6 to 9
move 8 from 8 to 6
move 1 from 7 to 4
move 5 from 2 to 4
move 7 from 5 to 1
move 5 from 8 to 9
move 11 from 6 to 7
move 9 from 9 to 1
move 2 from 7 to 5
move 1 from 9 to 5
move 1 from 3 to 6
move 3 from 4 to 6
move 1 from 8 to 2
move 2 from 3 to 6
move 6 from 5 to 2
move 3 from 5 to 9
move 3 from 2 to 1
move 1 from 4 to 3
move 3 from 2 to 7
move 1 from 8 to 9
move 1 from 2 to 8
move 8 from 7 to 5
move 1 from 7 to 8
move 3 from 5 to 6
move 5 from 5 to 2
move 1 from 4 to 1
move 1 from 3 to 2
move 4 from 1 to 5
move 4 from 2 to 6
move 6 from 1 to 2
move 5 from 9 to 3
move 2 from 5 to 3
move 3 from 3 to 6
move 10 from 6 to 4
move 4 from 8 to 5
move 5 from 5 to 1
move 21 from 1 to 7
move 3 from 2 to 9
move 1 from 5 to 2
move 4 from 2 to 9
move 8 from 4 to 8
move 1 from 2 to 1
move 7 from 8 to 2
move 2 from 6 to 1
move 2 from 1 to 5
move 1 from 1 to 5
move 4 from 3 to 7
move 1 from 9 to 3
move 4 from 6 to 3
move 1 from 3 to 8
move 1 from 3 to 4
move 2 from 2 to 6
move 2 from 9 to 7
move 14 from 7 to 8
move 10 from 8 to 7
move 3 from 4 to 6
move 5 from 2 to 3
move 3 from 9 to 8
move 3 from 3 to 4
move 1 from 2 to 4
move 1 from 9 to 4
move 1 from 9 to 5
move 1 from 5 to 2
move 3 from 5 to 7
move 1 from 4 to 6
move 5 from 3 to 8
move 1 from 6 to 8
move 5 from 7 to 6
move 14 from 8 to 5
move 2 from 6 to 7
move 18 from 7 to 2
move 3 from 6 to 1
move 5 from 5 to 4
move 5 from 6 to 2
move 7 from 2 to 1
move 1 from 8 to 4
move 1 from 5 to 1
move 8 from 1 to 9
move 10 from 4 to 3
move 8 from 5 to 3
move 1 from 4 to 3
move 2 from 1 to 5
move 1 from 5 to 3
move 5 from 3 to 1
move 1 from 1 to 3
move 5 from 1 to 6
move 13 from 3 to 1
move 3 from 9 to 4
move 2 from 9 to 6
move 5 from 6 to 5
move 6 from 5 to 1
move 7 from 7 to 9
move 7 from 9 to 6
move 1 from 9 to 3
move 1 from 7 to 9
move 3 from 9 to 1
move 12 from 2 to 7
move 7 from 6 to 2
move 22 from 1 to 7
move 1 from 6 to 5
move 4 from 7 to 6
move 1 from 5 to 6
move 2 from 4 to 1
move 1 from 4 to 1
move 23 from 7 to 9
move 4 from 6 to 2
move 4 from 7 to 3
move 1 from 1 to 9
move 6 from 2 to 1
move 1 from 7 to 2
move 7 from 2 to 8
move 2 from 3 to 8
move 3 from 1 to 9
move 1 from 2 to 8
move 5 from 8 to 3
move 3 from 2 to 1
move 2 from 7 to 8
move 10 from 9 to 8
move 4 from 1 to 3
move 14 from 3 to 4
move 7 from 4 to 5
move 1 from 6 to 9
move 5 from 5 to 8
move 1 from 6 to 4
move 6 from 9 to 4
move 3 from 8 to 4
move 1 from 5 to 1
move 3 from 4 to 3
move 9 from 4 to 3
move 5 from 3 to 6
move 5 from 1 to 5
move 4 from 6 to 2
move 8 from 9 to 2
move 2 from 6 to 5
move 3 from 4 to 7
move 2 from 2 to 7
move 2 from 5 to 4
move 3 from 5 to 9
move 3 from 4 to 2
move 10 from 2 to 5
move 1 from 9 to 8
move 2 from 2 to 9
move 3 from 7 to 2
move 1 from 2 to 9
move 13 from 5 to 1
move 2 from 2 to 7
move 8 from 9 to 2
move 1 from 4 to 6
move 1 from 9 to 5
move 14 from 8 to 4
move 7 from 4 to 5
move 4 from 7 to 5
move 2 from 3 to 8
move 4 from 1 to 5
move 2 from 5 to 4
move 6 from 5 to 6
move 7 from 2 to 5
move 1 from 2 to 6
move 1 from 5 to 2
move 2 from 2 to 8
move 2 from 1 to 3
move 8 from 4 to 7
move 1 from 4 to 3
move 6 from 1 to 6
move 7 from 3 to 9
move 3 from 7 to 1
move 2 from 8 to 7
move 7 from 6 to 9
move 2 from 3 to 6
move 6 from 8 to 3
move 9 from 5 to 3
move 2 from 7 to 8
move 2 from 6 to 4
move 7 from 6 to 9
move 5 from 3 to 8
move 10 from 9 to 1
move 11 from 1 to 8
move 1 from 3 to 2
move 4 from 5 to 6
move 2 from 6 to 2
move 2 from 7 to 9
move 3 from 1 to 7
move 6 from 3 to 9
move 2 from 7 to 2
move 2 from 6 to 9
move 1 from 5 to 9
move 11 from 9 to 8
move 1 from 4 to 5
move 6 from 9 to 8
move 31 from 8 to 9
move 1 from 3 to 6
move 1 from 7 to 1
move 1 from 4 to 3
move 1 from 5 to 2
move 1 from 1 to 8
move 1 from 8 to 9
move 1 from 7 to 3
move 11 from 9 to 6
move 2 from 3 to 1
move 2 from 3 to 5
move 1 from 5 to 4
move 1 from 4 to 1
move 6 from 8 to 3
move 1 from 1 to 4
move 1 from 4 to 6
move 2 from 3 to 6
move 17 from 9 to 2
move 23 from 2 to 9
move 14 from 9 to 4
move 1 from 1 to 7
move 1 from 5 to 6
move 8 from 6 to 2
move 1 from 3 to 2
move 4 from 9 to 8
move 5 from 4 to 7
move 3 from 7 to 2
move 1 from 1 to 2
move 2 from 9 to 4
move 3 from 6 to 9
move 8 from 4 to 9
move 2 from 4 to 2
move 4 from 7 to 2
move 1 from 7 to 9
move 4 from 6 to 2
move 16 from 2 to 1
move 2 from 3 to 2
move 18 from 9 to 8
move 1 from 4 to 2
move 1 from 6 to 8
move 1 from 3 to 9
move 3 from 9 to 5
move 4 from 9 to 8
move 6 from 2 to 8
move 1 from 5 to 1
move 4 from 2 to 8
move 1 from 5 to 1
move 17 from 1 to 4
move 1 from 5 to 8
move 10 from 4 to 3
move 10 from 3 to 1
move 4 from 4 to 9
move 1 from 4 to 6
move 1 from 4 to 8
move 38 from 8 to 1
move 27 from 1 to 5
move 1 from 8 to 2
move 1 from 6 to 3
move 1 from 4 to 8
move 1 from 8 to 4
move 14 from 1 to 9
move 1 from 3 to 1
move 1 from 5 to 1
move 1 from 2 to 5
move 2 from 5 to 4
move 17 from 5 to 8
move 3 from 4 to 9
move 2 from 9 to 1
move 3 from 5 to 7
move 3 from 7 to 4
move 2 from 4 to 7
move 12 from 1 to 4
move 1 from 7 to 4
move 1 from 7 to 6
move 1 from 6 to 9
move 11 from 4 to 3
move 1 from 5 to 3
move 11 from 3 to 9
move 1 from 3 to 2
move 3 from 5 to 4
move 1 from 2 to 4
move 1 from 5 to 8
move 13 from 9 to 3
move 16 from 9 to 1
move 4 from 8 to 9
move 2 from 1 to 4
move 1 from 9 to 1
move 1 from 9 to 7
move 1 from 7 to 2
move 6 from 8 to 3
move 8 from 4 to 2
move 4 from 9 to 6
move 3 from 2 to 3
move 3 from 6 to 1
move 3 from 8 to 6
move 1 from 6 to 8
move 3 from 6 to 4
move 11 from 3 to 5
move 4 from 8 to 2
move 6 from 3 to 5
move 3 from 5 to 1
move 2 from 8 to 3
move 14 from 5 to 3
move 4 from 3 to 4
move 6 from 3 to 5
move 3 from 2 to 9
move 4 from 1 to 8
move 3 from 9 to 6
move 2 from 6 to 9
move 6 from 4 to 3
move 15 from 1 to 4
move 1 from 6 to 7
move 5 from 5 to 1
move 11 from 3 to 1
move 2 from 9 to 7
move 1 from 5 to 6
move 2 from 1 to 3
move 7 from 2 to 6
move 4 from 8 to 1
move 8 from 4 to 2
move 3 from 6 to 4
move 5 from 1 to 4
move 17 from 4 to 8
move 3 from 3 to 7
move 4 from 3 to 4
move 4 from 4 to 2
move 9 from 8 to 7
move 1 from 3 to 8
move 10 from 2 to 4
move 1 from 6 to 2
move 2 from 8 to 4
move 2 from 6 to 9
move 2 from 6 to 2
move 1 from 2 to 3
move 3 from 1 to 4
move 1 from 3 to 2
move 1 from 9 to 3
move 1 from 9 to 7
move 4 from 8 to 4
move 10 from 4 to 8
move 5 from 4 to 3
move 1 from 2 to 8
move 5 from 3 to 7
move 3 from 7 to 8
move 3 from 4 to 3
move 8 from 7 to 2
move 8 from 7 to 8
move 1 from 3 to 2
move 3 from 2 to 8
move 9 from 2 to 5
move 12 from 1 to 7
move 21 from 8 to 3
move 5 from 8 to 6
move 8 from 7 to 5
move 6 from 7 to 4
move 12 from 5 to 7
move 1 from 8 to 5
move 2 from 4 to 2
move 1 from 7 to 6
move 14 from 3 to 8
move 5 from 6 to 2
move 7 from 2 to 6
move 6 from 8 to 4
move 11 from 7 to 4
move 8 from 3 to 7
move 4 from 5 to 7
move 9 from 8 to 2
move 6 from 4 to 1
move 2 from 5 to 2
move 1 from 7 to 2
move 11 from 2 to 3
move 1 from 2 to 1
move 7 from 4 to 1
move 5 from 6 to 8
move 1 from 2 to 3
move 2 from 8 to 7
move 14 from 3 to 7
move 15 from 7 to 6
move 4 from 4 to 6
move 2 from 8 to 3
move 12 from 1 to 3
move 1 from 8 to 2
move 1 from 2 to 3
move 1 from 3 to 9
move 1 from 9 to 7
move 1 from 1 to 4
move 18 from 6 to 8
move 3 from 3 to 2
move 17 from 8 to 3
move 3 from 7 to 6
move 3 from 2 to 6
move 25 from 3 to 7
move 2 from 4 to 1
move 9 from 6 to 5
move 2 from 3 to 1
move 1 from 3 to 9
move 5 from 5 to 2
move 1 from 8 to 3
move 2 from 4 to 7
move 1 from 9 to 4
move 1 from 6 to 7
move 2 from 5 to 2
move 2 from 4 to 8
move 2 from 5 to 8
move 5 from 7 to 9
move 27 from 7 to 5
move 2 from 9 to 6
"""
