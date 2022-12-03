namespace AdventOfCode2022.Inputs

open System

[<AutoOpen>]
module Common =
    let parseInput (input : string) = input.Split Environment.NewLine
    let parseInputMapped mapper (input : string) = input |> parseInput |> Array.map mapper
