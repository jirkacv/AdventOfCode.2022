namespace AdventOfCode2022.Days

open AdventOfCode2022.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module Day03 =

    let [<Literal>] Letters = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let letterArray = Letters.ToCharArray()

    let priority (letter : char) = System.Array.IndexOf(letterArray, letter)

    let commonItemPriority (row : string) =
        let chars = row.ToCharArray()
        let half = (chars |> Array.length) / 2
        let left, right = chars |> Array.splitAt half
        let commonChar =
            left
            |> Array.find (fun leftChar -> right |> Array.exists (fun rightChar -> rightChar = leftChar))
        commonChar
        |> priority

    let part1 input =
        input
        |> Array.map commonItemPriority
        |> Array.sum

    let groupBadgeItemPriority (rows : string[]) =
        let sets = rows |> Array.map (fun row -> row.ToCharArray() |> Set.ofArray)
        let badge = sets |> Set.intersectMany
        let char = badge |> Seq.head
        char |> priority

    let part2 input =
        input
        |> Array.chunkBySize 3
        |> Array.map groupBadgeItemPriority
        |> Array.sum


    type Tests(output:ITestOutputHelper) =
        [<Fact>]
        let testPart1() =
            loadInput Test 3 |> part1 |> should equal 157

        [<Fact>]
        let taskPart1() =
            loadInput Task 3 |> part1 |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            loadInput Test 3 |> part2 |> should equal 70

        [<Fact>]
        let taskPart2() =
            loadInput Task 3 |> part2 |> string |> output.WriteLine
