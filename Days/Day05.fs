namespace AdventOfCode2022.Days

open System.Collections.Generic
open System.Text.RegularExpressions
open Xunit.Abstractions

module Day05 =
    let [<Literal>] MoveRegexPattern = "move (\d+) from (\d+) to (\d+)"
    let private moveRegex = Regex(MoveRegexPattern)

    type Crates = Map<int, Stack<char>>
    type Move =
        {
            Count : int
            From : int
            To : int
        }
        static member ofLineString line =
            match moveRegex.Match line with
            | matchResult when matchResult.Success ->
                {
                    Count = matchResult.Groups[1].Value |> int
                    From = matchResult.Groups[2].Value |> int
                    To = matchResult.Groups[3].Value |> int
                }
            | _ -> failwith "Couldn't parse Move from line string "

    let private crateCharacterIndex pos = (pos * 4) - 3

    let private parseCrates (cratesWithRownums : string[]) =
        let rowNumsArr, cratesArr = cratesWithRownums |> Array.rev |> Array.splitAt 1
        let rowCount = (rowNumsArr |> Array.exactlyOne).Split " " |> Array.last |> int

        let crates =
            Array.init rowCount (fun i -> i + 1, Stack<char>())
            |> Map.ofSeq

        cratesArr
        |> Array.map (fun row ->
            for crateNumber = 1 to crates.Count do
                let crateValue = row.ToCharArray() |> Array.tryItem (crateCharacterIndex crateNumber)
                match crateValue with
                | None | Some ' ' -> ()
                | Some value ->
                    let crate = crates[crateNumber]
                    crate.Push value
            )
        |> ignore
        crates

    let private parseInput input =
        let separatorIndex = input |> Array.findIndex (fun ln -> ln = "")
        let cratesWithRownumsArr, movesArr = input |> Array.splitAt separatorIndex

        let crates = parseCrates cratesWithRownumsArr
        let moves = movesArr |> Array.skip 1
        crates, moves |> Array.map Move.ofLineString

    let private rearrangeCrates9000 (crates : Crates, moves : Move[]) =
        moves
        |> Array.iter (fun move ->
            let srcCrate = crates[move.From]
            let destCrate = crates[move.To]
            for _ = 1 to move.Count do
                let value = srcCrate.Pop()
                destCrate.Push value
            )
        crates

    let private topCrateValues (crates : Crates) =
        crates
        |> Seq.map (fun kv -> kv.Value.Pop() |> string)
        |> String.concat ""

    let part1 input =
        input
        |> parseInput
        |> rearrangeCrates9000
        |> topCrateValues

    let private rearrangeCrates9001 (crates : Crates, moves : Move[]) =
        moves
        |> Array.iter (fun move ->
            let srcCrate = crates[move.From]
            let destCrate = crates[move.To]
            let values = [| for _ = 1 to move.Count do srcCrate.Pop() |] |> Array.rev
            values |> Array.iter destCrate.Push )
        crates

    let part2 input =
        input
        |> parseInput
        |> rearrangeCrates9001
        |> topCrateValues

    type Tests(output:ITestOutputHelper) =
        inherit DayTests.Tests<string>(output, 5, part1, "CMZ", part2, "MCD" )
