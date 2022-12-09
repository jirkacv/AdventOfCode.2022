namespace AdventOfCode2022.Days

open System
open Xunit.Abstractions

module Day08 =

    type Around<'t> =
        {
            Up : 't
            Right : 't
            Down : 't
            Left : 't
        }

    type TreesAround = Around<byte[]>

    type TreeRows = byte[] list

    let private columnItems col (rows : TreeRows)  =
        rows
        |> List.map (Array.item col)
        |> Array.ofList

    let private treesAround rowsBelow currentRow rowsAbove col =
        let left, right = currentRow |> Array.splitAt col
        let left = left |> Array.rev
        let right = right |> Array.skip 1
        let up = rowsAbove |> columnItems col |> Array.rev
        let down = rowsBelow |> columnItems col
        {
            Up = up
            Right = right
            Down = down
            Left = left
        }

    let private treeVisibleInDirection treeHeights currentHeight =
        match treeHeights with
        | [||] -> true
        | treeHeights -> treeHeights |> Array.max < currentHeight

    let part1 (input : string[]) =
        let trees = input |> Array.map (fun s -> s |> Array.ofSeq |> Array.map (Char.GetNumericValue >> byte)) |> List.ofSeq
        let maxCol = (trees |> List.head |> Array.length) - 1

        let mutable visibleCount = 0
        let mutable aboveRows : TreeRows = []
        let rec processRows (trees : TreeRows) =
            match trees with
            | [] -> ()
            | currentRow :: belowRows ->
                for col = 0 to maxCol do
                    let currentHeight = currentRow[col]
                    let around = treesAround belowRows currentRow aboveRows col
                    if
                        treeVisibleInDirection around.Up currentHeight ||
                        treeVisibleInDirection around.Right currentHeight ||
                        treeVisibleInDirection around.Down currentHeight ||
                        treeVisibleInDirection around.Left currentHeight
                    then visibleCount <- visibleCount + 1
                aboveRows <- List.append aboveRows [ currentRow ]
                processRows belowRows
        processRows trees
        visibleCount

    let private viewingDistance treeHeights currentHeight =
        match treeHeights with
        | [||] -> 0
        | treeHeights ->
            let mutable count = 0
            let rec processHeights heights =
                match heights with
                | [] -> ()
                | height :: tail ->
                    if height < currentHeight then
                        count <- count + 1
                        processHeights tail
                    else
                        count <- count + 1
            treeHeights |> List.ofArray |> processHeights
            count

    let part2 input =
        let trees = input |> Array.map (fun s -> s |> Array.ofSeq |> Array.map (Char.GetNumericValue >> byte)) |> List.ofSeq
        let maxCol = (trees |> List.head |> Array.length) - 1

        let mutable highestScenicScore = 0
        let mutable aboveRows : TreeRows = []
        let rec processRows (trees : TreeRows) =
            match trees with
            | [] -> ()
            | currentRow :: belowRows ->
                for col = 0 to maxCol do
                    let currentHeight = currentRow[col]
                    let around = treesAround belowRows currentRow aboveRows col
                    let scenicScore =
                        (viewingDistance around.Up currentHeight) *
                        (viewingDistance around.Right currentHeight) *
                        (viewingDistance around.Down currentHeight) *
                        (viewingDistance around.Left currentHeight)
                    if scenicScore > highestScenicScore then
                        highestScenicScore <- scenicScore
                aboveRows <- List.append aboveRows [ currentRow ]
                processRows belowRows
        processRows trees
        highestScenicScore

    type Tests(output:ITestOutputHelper) =
        inherit DayTests.Tests<int>(output, 8, part1, 21, part2, 8 )
