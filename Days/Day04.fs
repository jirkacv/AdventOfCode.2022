namespace AdventOfCode2022.Days

open Xunit.Abstractions

module Day04 =

    type Range =
        {
            Start : int
            End : int
        }

    let private parseRange (range : string) =
        let range = range.Split "-" |> Array.map int
        { Start = range[0]; End = range[1] }

    let private parseRanges (row : string) =
        let rowRanges = row.Split ","
        rowRanges[0] |> parseRange,
        rowRanges[1] |> parseRange

    let private rangeFitsInSourceRange (sourceRange : Range) (checkedRange : Range) =
        sourceRange.Start <= checkedRange.Start && sourceRange.End >= checkedRange.End

    let private eitherRangeContainsTheOther range1 range2 =
        rangeFitsInSourceRange range1 range2 || rangeFitsInSourceRange range2 range1

    let part1 input =
        input
        |> Array.map parseRanges
        |> Array.filter (fun (range1, range2) -> eitherRangeContainsTheOther range1 range2)
        |> Array.length


    let private rangeOverlapsSourceRange (sourceRange : Range) (checkedRange : Range) =
        sourceRange.Start <= checkedRange.End && checkedRange.Start <= sourceRange.End

    let part2 input =
        input
        |> Array.map parseRanges
        |> Array.filter (fun (range1, range2) -> rangeOverlapsSourceRange range1 range2)
        |> Array.length

    type Tests(output:ITestOutputHelper) =
        inherit DayTests.Tests<int>(output, 4, part1, 2, part2, 4 )
