namespace AdventOfCode2022.Days

open AdventOfCode2022.Inputs
open Xunit
open Xunit.Abstractions
open FsUnit.Xunit

module DayTests =

    [<AbstractClass>]
    type Tests<'r>(
        output:ITestOutputHelper,
        day : int,
        part1fn : string[] -> 'r,
        part1result : 'r,
        part2fn : string[] -> 'r,
        part2result : 'r) =

        [<Fact>]
        let testPart1() =
            loadInput Test day |> part1fn |> should equal part1result

        [<Fact>]
        let taskPart1() =
            loadInput Task day |> part1fn |> string |> output.WriteLine

        [<Fact>]
        let testPart2() =
            loadInput Test day |> part2fn |> should equal part2result

        [<Fact>]
        let taskPart2() =
            loadInput Task day |> part2fn |> string |> output.WriteLine
