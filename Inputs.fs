namespace AdventOfCode2022

open System

module Inputs =

    type InputType =
        | Test
        | Task

    let loadInput inputType day = $"Inputs/%A{inputType}%02i{day}.txt" |> IO.File.ReadAllLines

    let loadInputMapped inputType day mapper = loadInput inputType day |> Array.map mapper
