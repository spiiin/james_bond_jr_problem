open System
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

let tryAddOrUpdate (explored: ConcurrentDictionary<State, int>) (state: State) (rate: int) =
    match explored.TryGetValue(state) with
    | true, currentRate when currentRate <= rate -> false
    | _ -> 
        explored.[state] <- rate
        true

let rec nextSearchStep (transformPaths: Path array -> Path array) (filterPath: Path -> bool) (rate: State -> State -> int) (paths: Path array) (explored: ConcurrentDictionary<State, int>) (endState: State) : seq<Path array> = 
    seq {
        if Array.isEmpty paths then
            yield! Array.empty
        else
            let sortedPaths = transformPaths paths
            let more =
                sortedPaths
                |> PSeq.withDegreeOfParallelism Environment.ProcessorCount
                |> PSeq.collect (fun path ->
                    moves
                    |> Seq.map (fun move -> path.Extend(move, endState, rate))
                    |> Seq.filter filterPath
                )
                |> PSeq.toArray

            printfn $"currentMoves : {sortedPaths.Length} nextMoves : {Array.length more} explored : {explored.Count}"  
            more
            |> PSeq.withDegreeOfParallelism Environment.ProcessorCount
            |> PSeq.iter (fun path -> tryAddOrUpdate explored path.State path.DiffCount |> ignore)

            yield sortedPaths
            yield! nextSearchStep transformPaths filterPath rate more explored endState
    }

let transformPaths (paths: Path array) : Path array =
    paths |> Array.sortBy (fun path -> path.DiffCount) |> Array.truncate 5000

let filterPaths (explored: ConcurrentDictionary<State, int>) (maxPathLength: int) (path: Path) =
    match explored.TryGetValue(path.State) with
    | true, currentRate when currentRate <= path.DiffCount -> false
    | _ -> List.length path.Path <= maxPathLength

let rate (State s1) (State s2) =
    let getWrapDistance a b size = min ((a - b + size) % size) ((b - a + size) % size)
    [0..15]
    |> List.sumBy (fun i ->
        if s1.[i] = s2.[i] then 0 else
        let row, col = i / 4, i % 4
        let target = s2.[i]
        let rowShifts =
            [0..3]
            |> List.choose (fun j ->
                if s1.[row * 4 + j] = target then Some (getWrapDistance j col 4)
                else None)
        let colShifts =
            [0..3]
            |> List.choose (fun j ->
                if s1.[j * 4 + col] = target then Some (getWrapDistance j row 4)
                else None)
        match rowShifts, colShifts with
        | [], [] -> 2
        | rs, [] -> List.min rs
        | [], cs -> List.min cs
        | rs, cs -> min (List.min rs) (List.min cs)
    )

let initialState = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
let endState     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]

let initialPaths = [| Path([], initialState, Int32.MaxValue) |]

let initialExplored = ConcurrentDictionary<State, int>(StateEqualityComparer())
initialExplored.TryAdd(initialState, Int32.MaxValue) |> ignore

let nextSearchStepCurry = nextSearchStep transformPaths (filterPaths initialExplored 8) rate
let pathSets = nextSearchStepCurry initialPaths initialExplored endState

let solution = pathSets |> Seq.collect id |> Seq.filter (fun path -> path.State = endState)
let printSolution (p: seq<Path>) = p |> Seq.truncate 1 |> Seq.iter (fun path -> printfn $"{path}")
printSolution solution

System.Console.ReadKey() |> ignore