namespace AdventOfCode2022.Days

open System.Collections.Generic
open Xunit.Abstractions

module Day06 =

    let [<Literal>] PacketMarkerLength = 4
    let [<Literal>] MessageMarkerLength = 14

    let rec private onlyUniqueItems (queue : Queue<'t>) =
        (queue.ToArray() |> Array.distinct |> Array.length) = queue.Count

    let private packetMarkerIndex (markerLength : int) (signal : string) =
        let q = Queue<char>(markerLength)
        let rec markerIndex currentIndex =
            if q.Count = markerLength then q.Dequeue() |> ignore
            q.Enqueue signal[currentIndex]
            if not (q.Count = markerLength && onlyUniqueItems q)
            then markerIndex (currentIndex + 1)
            else currentIndex + 1
        markerIndex 0

    let part1 input =
        input
        |> Array.head
        |> packetMarkerIndex PacketMarkerLength

    let part2 input =
        input
        |> Array.head
        |> packetMarkerIndex MessageMarkerLength

    type Tests(output:ITestOutputHelper) =
        inherit DayTests.Tests<int>(output, 6, part1, 7, part2, 19 )
