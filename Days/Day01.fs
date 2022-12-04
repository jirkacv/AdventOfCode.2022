namespace AdventOfCode2022.Days

open System
open Xunit.Abstractions

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
        inherit DayTests.Tests<int>(output, 1, part1, 24000, part2, 45000 )
