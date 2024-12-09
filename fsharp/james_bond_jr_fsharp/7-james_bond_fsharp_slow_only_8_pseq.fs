﻿open System
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Collections.ParallelSeq

type State = State of int[]

type StateEqualityComparer() =
    interface IEqualityComparer<State> with
        member _.Equals((State x), (State y)) = Array.length x = Array.length y && Array.forall2 (=) x y
        member _.GetHashCode(State x) = x |> Array.fold (fun acc elem -> acc * 31 + elem) 17

type Move(description: string, direction: int -> int, changeChecker: int -> bool) =
    override this.ToString() = description
    member this.Act(State state) =
        State (
            Array.init 16 (fun i ->
                if changeChecker i then state.[direction i]
                else state.[i]
            )
        )

let moveLeft row = Move($"MoveLeft {row}", (fun i -> row * 4 + (i + 1) % 4), (fun i -> i / 4 = row))
let moveRight row = Move($"MoveRight {row}", (fun i -> row * 4 + (i + 3) % 4), (fun i -> i / 4 = row))
let moveUp col = Move($"MoveUp {col}", (fun i -> (i + 4) % 16), (fun i -> i % 4 = col))
let moveDown col = Move($"MoveDown {col}", (fun i -> (i + 12) % 16), (fun i -> i % 4 = col))
let moves = [0 .. 3] |> List.collect (fun i -> [ moveLeft i; moveRight i; moveUp i; moveDown i ])

type Path(path: Move list, state: State, diffCount: int) =
    member this.Path = path
    member this.State = state
    member this.DiffCount = diffCount
    member this.Extend(move: Move, endState: State, rate: State -> State -> int) =
        let newState = move.Act(state)
        let newDiffCount = rate newState endState
        Path(move :: path, newState, newDiffCount)
    override this.ToString() = path |> List.rev |> List.map string |> String.concat "\n"

let rec nextSearchStep (transformPaths: Path list -> Path list) (filterPath: Path -> bool) (paths: Path list) (explored: ConcurrentDictionary<State, unit>) (endState: State) : seq<Path list> = 
    seq {
        if List.isEmpty paths then
            yield! Seq.empty
        else
            let sortedPaths = transformPaths paths
            let rate (State s1) (State s2) = Array.fold2 (fun acc x y -> if x <> y then acc + 1 else acc) 0 s1 s2
            let more =
                sortedPaths
                |> PSeq.withDegreeOfParallelism Environment.ProcessorCount
                |> PSeq.collect (fun path ->
                    moves
                    |> Seq.map (fun move -> path.Extend(move, endState, rate))
                    |> Seq.filter filterPath
                )
                |> Seq.toList

            printfn $"currentMoves : {sortedPaths.Length} nextMoves : {List.length more} explored : {explored.Count}"  
            more
            |> PSeq.withDegreeOfParallelism Environment.ProcessorCount
            |> PSeq.iter (fun path -> explored.TryAdd(path.State, ()) |> ignore)

            yield sortedPaths
            yield! nextSearchStep transformPaths filterPath more explored endState
    }

let transformPaths (paths: Path list) : Path list =
    paths |> List.sortBy (fun path -> path.DiffCount) |> List.truncate 200000

let filterPaths (explored: ConcurrentDictionary<State, unit>) (maxPathLength: int) (path: Path) =
    not (explored.ContainsKey(path.State)) && List.length path.Path <= maxPathLength

let initialState = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
let endState     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]

let initialPaths = [Path([], initialState, Int32.MaxValue)]

let initialExplored = ConcurrentDictionary<State, unit>(StateEqualityComparer())
initialExplored.TryAdd(initialState, ()) |> ignore

let nextSearchStepCurry = nextSearchStep transformPaths (filterPaths initialExplored 8)
let pathSets = nextSearchStepCurry initialPaths initialExplored endState

let solution = pathSets |> Seq.collect id |> Seq.filter (fun path -> path.State = endState)
let printSolution (p: seq<Path>) = p |> Seq.truncate 1 |> Seq.iter (fun path -> printfn $"{path}")
printSolution solution

System.Console.ReadKey() |> ignore