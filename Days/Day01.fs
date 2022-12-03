namespace AdventOfCode2022.Days

open System
open AdventOfCode2022.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day01 =

    let private elfCalories input =
        Array.fold (fun elfs (current : string) ->
            match Int32.TryParse current with
            | true, calories ->
                let lastElf = elfs |> Array.last
                Array.set elfs (elfs.Length - 1) (lastElf + calories)
                elfs
            | _ ->
                Array.append elfs [| 0 |]
        ) [| 0 |] input

    let part1 input =
        input |> elfCalories |> Array.max

    let part2 input =
        input
        |> elfCalories
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum

    type Tests(output:ITestOutputHelper) =
        [<Fact>]
        let testPart1() =
            Inputs01.Test |> parseInput |> part1 |> should equal 24000

        [<Fact>]
        let taskPart1() =
            Inputs01.Task |> parseInput |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            Inputs01.Test |> parseInput |> part2 |> should equal 45000

        [<Fact>]
        let taskPart2() =
            Inputs01.Task |> parseInput |> part2 |> string |> output.WriteLine
