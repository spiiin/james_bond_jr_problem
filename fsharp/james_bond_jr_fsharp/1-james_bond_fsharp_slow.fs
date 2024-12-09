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

type Path(path: Move list, endState: State) =
    member this.EndState = endState
    member this.Extend(move: Move) = Path(move :: path, move.Act(endState))
    override this.ToString() = path |> List.rev |> List.map string |> String.concat "\n"

let rec next_search_step (paths: Path list) (explored: HashSet<State>) (endState: State): seq<Path list> =
    seq {
        if List.isEmpty paths then
            yield! Seq.empty
        else
            let comparePaths (p1: Path) (p2: Path) =
                let diffCount (State s1) =
                    Array.zip s1 (let (State es) = endState in es) //inplaced pattern-matching with captured endState
                    |> Array.sumBy (fun (x, y) -> if x <> y then 1 else 0)
                diffCount p1.EndState - diffCount p2.EndState //TODO: sort-function -> argument

            let sortedPaths = paths |> List.sortWith comparePaths |> List.truncate 10000 //TODO: 1. truncate-func -> argument. 2. partial-sort?

            let more =
                sortedPaths
                |> List.collect (fun path ->
                    moves
                    |> List.map path.Extend
                    |> List.filter (fun next -> not (explored.Contains(next.EndState)))
                )

            printfn $"pathSet.size = {List.length more} explored.size = {explored.Count}"  

            more |> List.iter (fun path -> explored.Add(path.EndState) |> ignore)

            yield sortedPaths
            yield! next_search_step more explored endState
    }

let initialState = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
let endState     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]

let initialPaths = [Path([], initialState)]
let initialExplored = HashSet<State>([initialState])
let pathSets = next_search_step initialPaths initialExplored endState

let solution = pathSets|> Seq.collect id |> Seq.filter (fun path -> path.EndState = endState)
let printSolution (p: seq<Path>) = p |> Seq.truncate 1 |> Seq.iter (fun path -> printfn $"{path}")
printSolution solution