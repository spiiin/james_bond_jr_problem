open System
open System.Collections.Generic
open System.Collections.Concurrent
open FSharp.Collections.ParallelSeq
open FSharpx
open System.IO

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

type Env = {
    TransformPaths: Path array -> Path array
    FilterPath: Path -> bool
    Rate: State -> State -> int
    Explored: ConcurrentDictionary<State, int>
    EndState: State
}

let tryAddOrUpdate (explored: ConcurrentDictionary<State, int>) (state: State) (rate: int) =
    match explored.TryGetValue(state) with
    | true, currentRate when currentRate <= rate -> false
    | _ -> 
        explored.[state] <- rate
        true

let rec nextSearchStep (paths: Path array) =
    Reader.reader {
        let! { TransformPaths = transformPaths; FilterPath = filterPath; Rate = rate; Explored = explored; EndState = endState } = Reader.ask

        if Array.isEmpty paths then
            return Seq.empty
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

            let! nextStepResult = nextSearchStep more
            return Seq.append (Seq.singleton sortedPaths) nextStepResult
    }

let transformPaths (paths: Path array) : Path array =
    paths |> Array.sortBy (fun path -> path.DiffCount) |> Array.truncate 5000

let filterPaths (explored: ConcurrentDictionary<State, int>) (maxPathLength: int) (path: Path) =
    match explored.TryGetValue(path.State) with
    | true, currentRate when currentRate <= path.DiffCount -> false
    | _ -> List.length path.Path <= maxPathLength


//---
let loadRatings (filePath: string) : int[] =
    use fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read)
    use binaryReader = new BinaryReader(fileStream)
    let fileLength = int fileStream.Length
    let ratings = Array.zeroCreate fileLength
    for i in 0 .. fileLength - 1 do
        ratings.[i] <- int (binaryReader.ReadByte())  
    ratings

let loadDictionary (filePath: string) : Dictionary<int, int> =
    let dictionary = Dictionary<int, int>()
    use fileStream = new FileStream(filePath, FileMode.Open, FileAccess.Read)
    use binaryReader = new BinaryReader(fileStream)
    while fileStream.Position < fileStream.Length do
        let packedState = binaryReader.ReadInt32()
        let index = binaryReader.ReadInt32()
        dictionary.Add(packedState, index)
    dictionary

let stateToPackedInt (state: int[]) : int =
    state
    |> Array.mapi (fun idx value -> (value - 1) <<< (idx * 2))
    |> Array.fold (|||) 0

let precalculatedRate = loadRatings "ratings_4342312442432431.bin"
let dictionary = loadDictionary "dictionary_4342312442432431.bin"
let rate (State s1) (State s2) =
    let packedIndex = stateToPackedInt s2
    match dictionary.TryGetValue(packedIndex) with
        | true, index -> precalculatedRate.[index]
        | false, _ -> failwith $"Precalculated value for {s1} doesn't exists"
//---

let initialState = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
let endState     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]

let initialPaths = [| Path([], initialState, Int32.MaxValue) |]

let initialExplored = ConcurrentDictionary<State, int>(StateEqualityComparer())
initialExplored.TryAdd(initialState, Int32.MaxValue) |> ignore

let env = {
    TransformPaths = transformPaths
    FilterPath = filterPaths initialExplored 8
    Rate = rate
    Explored = initialExplored
    EndState = endState
}

let sw = System.Diagnostics.Stopwatch.StartNew()
let pathSets = nextSearchStep initialPaths env
let solution = pathSets |> Seq.collect id |> Seq.filter (fun path -> path.State = endState)
sw.Stop()
printfn $"Elapsed: {sw.Elapsed}"

let printSolution (p: seq<Path>) = p |> Seq.truncate 1 |> Seq.iter (fun path -> printfn $"{path}")
printSolution solution
System.Console.ReadKey() |> ignore