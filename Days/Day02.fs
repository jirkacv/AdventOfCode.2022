namespace AdventOfCode2022.Days

open System
open AdventOfCode2022.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day02 =

    type Symbol =
        | Rock
        | Paper
        | Scissors
        static member ofOpponentMove str =
            match str with
            | "A" -> Rock
            | "B" -> Paper
            | "C" -> Scissors
            | unknown -> failwith $"Can't parse opponent symbol '{unknown}'"
        static member ofMyMove str =
            match str with
            | "X" -> Rock
            | "Y" -> Paper
            | "Z" -> Scissors
            | unknown -> failwith $"Can't parse my move symbol '{unknown}'"
        member this.Score =
            match this with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3

    type Outcome =
        | Win
        | Loss
        | Draw
        static member score = function
            | Win -> 6
            | Loss -> 0
            | Draw -> 3
        static member ofString str =
            match str with
            | "X" -> Loss
            | "Y" -> Draw
            | "Z" -> Win
            | unknown -> failwith $"Can't parse outcome '{unknown}'"

    let resolveRound myMove opponentMove =
        match myMove, opponentMove with
        | Rock, Scissors
        | Paper, Rock
        | Scissors, Paper ->
            Win
        | myMove, opponentMove when myMove = opponentMove ->
            Draw
        | _ ->
            Loss

    let roundScore (myMove, opponentMove) =
        let roundScore =
            resolveRound myMove opponentMove
            |> Outcome.score
        roundScore + myMove.Score

    let part1 input =
        input
        |> Array.map roundScore
        |> Array.sum


    let symbolFromOutcome opponentMove outcome =
        match opponentMove, outcome with
        | move, Draw -> move
        | Rock, Win -> Paper
        | Paper, Win -> Scissors
        | Scissors, Win -> Rock
        | Rock, Loss -> Scissors
        | Paper, Loss -> Rock
        | Scissors, Loss -> Paper

    let part2 input =
        input
        |> Array.map (fun (opponentMove, outcome) ->
            let myMove = symbolFromOutcome opponentMove outcome
            myMove.Score + (outcome |> Outcome.score))
        |> Array.sum

    let part1mapper (row : string) =
        row[2] |> string |> Symbol.ofMyMove,
        row[0] |> string |> Symbol.ofOpponentMove

    let part2mapper (row : string) =
        row[0] |> string |> Symbol.ofOpponentMove,
        row[2] |> string |> Outcome.ofString

    type Tests(output:ITestOutputHelper) =
        [<Fact>]
        let testPart1() =
            Inputs02.Test |> parseInputMapped part1mapper |> part1 |> should equal 15

        [<Fact>]
        let taskPart1() =
            Inputs02.Task |> parseInputMapped part1mapper |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            Inputs02.Test |> parseInputMapped part2mapper |> part2 |> should equal 12

        [<Fact>]
        let taskPart2() =
            Inputs02.Task |> parseInputMapped part2mapper |> part2 |> string |> output.WriteLine
