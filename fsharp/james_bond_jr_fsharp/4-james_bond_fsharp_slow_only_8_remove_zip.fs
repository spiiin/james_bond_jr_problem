open System.Collections.Generic;

type State = State of int[]

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

type Path(path: Move list, state: State) =
    member this.Path = path
    member this.State = state
    member this.Extend(move: Move) = Path(move :: path, move.Act(state))
    override this.ToString() = path |> List.rev |> List.map string |> String.concat "\n"

let rec nextSearchStep (transformPaths: Path list -> Path list) (filterPath: Path -> bool) (paths: Path list) (explored: HashSet<State>) (endState: State) : seq<Path list> = 
    seq {
        if List.isEmpty paths then
            yield! Seq.empty
        else
            let sortedPaths = transformPaths paths
            let more =
                sortedPaths
                |> List.collect (fun path ->
                    moves
                    |> List.map path.Extend
                    |> List.filter filterPath
                )

            printfn $"currentMoves : {sortedPaths.Length} nextMoves : {List.length more} explored : {explored.Count}"  
            more |> List.iter (fun path -> explored.Add(path.State) |> ignore)

            yield sortedPaths
            yield! nextSearchStep transformPaths filterPath more explored endState
    }

let initialState = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
let endState     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]

let initialPaths = [Path([], initialState)]
let initialExplored = HashSet<State>([initialState])

let transformPaths (paths: Path list) : Path list =
    let diffCount (State s1) =
        let (State s2) = endState
        let mutable diff = 0
        let mutable i = 0
        let n = s1.Length
        while i < n do
            if s1.[i] <> s2.[i] then diff <- diff + 1
            i <- i + 1
        diff

    let comparePaths (p1: Path) (p2: Path) =
        diffCount p1.State - diffCount p2.State

    paths 
    |> List.sortWith comparePaths
    |> List.truncate 200000


let filterPaths (explored: HashSet<State>) (maxPathLength: int) (path: Path) =
    not (explored.Contains(path.State)) && List.length path.Path <= maxPathLength

let nextSearchStepCurry = nextSearchStep transformPaths (filterPaths initialExplored 8) //curry
let pathSets = nextSearchStepCurry initialPaths initialExplored endState

let solution = pathSets |> Seq.collect id |> Seq.filter (fun path -> path.State = endState)
let printSolution (p: seq<Path>) = p |> Seq.truncate 1 |> Seq.iter (fun path -> printfn $"{path}")
printSolution solution

0 |> ignore