namespace AdventOfCode2022.Days

open System.Collections.Generic
open Xunit.Abstractions

module Day07 =

    type NodeType =
        | File of size : int
        | Directory

    type Node =
        {
            Name : string
            Type : NodeType
        }

    let [<Literal>] PathSeparator = "/"
    type Path() =
        let path = Stack<string>()

        member _.ChangeDirectory dir = path.Push dir
        member _.Up() = path.Pop()
        member _.Root() = path.Clear()
        override this.ToString() =
            let dirs = path.ToArray() |> Array.rev |> String.concat PathSeparator
            let result = PathSeparator + dirs
            if path.Count > 0 then
                result + PathSeparator
            else
                result

    type State =
        {
            CurrentPath : Path
            Nodes : Dictionary<string, Node>
        }
        static member createRoot () =
            {
                CurrentPath = Path()
                Nodes =
                    [| KeyValuePair(PathSeparator, { Node.Name = PathSeparator; Type = NodeType.Directory }) |]
                    |> Dictionary<string, Node>
            }

    type Target =
        | Root
        | Up
        | Directory of name : string

    type Command =
        | List
        | ChangeDirectory of Target
        static member ofStrings (strings : string list) =
            match strings[0] with
            | "ls" -> List
            | "cd" ->
                match strings[1] with
                | ".." -> Up |> ChangeDirectory
                | PathSeparator -> Root |> ChangeDirectory
                | dir -> dir |> Directory |> ChangeDirectory
            | unknown -> failwith $"Unknown command {unknown}"

    type Row =
        | Command of Command
        | File of size : int * name : string
        | Directory of name : string

    let private parseRow (row : string) =
        match row.Split " " |> List.ofArray with
        | "$" :: tail -> tail |> Command.ofStrings |> Command
        | "dir" :: tail -> tail[0] |> Directory
        | [ fileSize; fileName ] -> File (fileSize |> int, fileName)
        | _ -> failwith "Unexpected row content"

    let private processRow state row =
        match row with
        | Command cmd ->
            match cmd with
            | ChangeDirectory target ->
                match target with
                | Root -> state.CurrentPath.Root()
                | Up -> state.CurrentPath.Up() |> ignore
                | Target.Directory dir -> state.CurrentPath.ChangeDirectory dir
            | List -> () // handled by next rows
        | Directory name ->
            let node =
                {
                    Node.Name = name
                    Type = NodeType.Directory
                }
            state.Nodes.Add ($"{state.CurrentPath}{name}", node)
            ()
        | File (size, name) ->
            let node =
                {
                    Node.Name = name
                    Type = NodeType.File size
                }
            state.Nodes.Add ($"{state.CurrentPath}{name}", node) |> ignore

    let rec private processRows (state : State) (rows : Row list) =
        match rows with
        | [] -> ()
        | [ head ] ->
            processRow state head
        | head :: tail ->
            processRow state head
            processRows state tail

    let private allDirsWithSize state =
        let dirNames =
            state.Nodes
            |> Seq.filter (fun kv -> kv.Value.Type = NodeType.Directory)
            |> Seq.map (fun kv -> kv.Key)
        dirNames
        |> Seq.map (fun dir ->
            dir,
            state.Nodes
            |> Seq.sumBy (fun kv ->
                match (kv.Key + PathSeparator).StartsWith dir, kv.Value.Type with
                | true, NodeType.File size ->
                    size
                | false, _
                | true, NodeType.Directory ->
                    0))

    let part1 input =
        let rows = input |> Array.map parseRow |> List.ofArray
        let state = State.createRoot()
        processRows state rows

        state
        |> allDirsWithSize
        |> Seq.filter (fun (_, size) -> size <= 100000 )
        |> Seq.sumBy snd

    let [<Literal>] TotalSpace = 70000000
    let [<Literal>] UpdateSize = 30000000

    let part2 input =
        let rows = input |> Array.map parseRow |> List.ofArray
        let state = State.createRoot()
        processRows state rows

        let dirsWithSizes = allDirsWithSize state
        let usedSpace =
            dirsWithSizes
            |> Seq.find (fun (name, _) -> name = PathSeparator)
            |> snd
        let requiredSpace = UpdateSize - (TotalSpace - usedSpace)

        dirsWithSizes
        |> Seq.sortBy snd
        |> Seq.find (fun (_, size) -> size >= requiredSpace)
        |> snd

    type Tests(output:ITestOutputHelper) =
        inherit DayTests.Tests<int>(output, 7, part1, 95437, part2, 24933642 )
