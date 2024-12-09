module MyProvider2Implementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes

open System.Collections.Concurrent
open FSharp.Collections.ParallelSeq
open FSharpx

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

            //printfn $"currentMoves : {sortedPaths.Length} nextMoves : {Array.length more} explored : {explored.Count}"  
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

[<TypeProvider>]
type RateProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("MyProvider2.DesignTime", "MyProvider2.Runtime")])

    let ns = "MyProvider2"
    let asm = Assembly.GetExecutingAssembly()
    do assert (typeof<DataSource>.Assembly.GetName().Name = asm.GetName().Name)  

    let createType typeName (initialStatePackerd: string, endStatePacked: string) =
        let asm = ProvidedAssembly()
        let myType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, isErased=false)

        let ctor = ProvidedConstructor([], invokeCode = fun args -> <@@ 0 @@>)
        myType.AddMember(ctor)

        //let initialState2 = State [| 1; 2; 2; 1;    3; 4; 4; 3;    3; 4; 4; 3;    2; 4; 4; 2 |]
        //let endState2     = State [| 4; 3; 4; 2;    3; 1; 2; 4;    4; 2; 4; 3;    2; 4; 3; 1 |]
        let parseState (state: string) : int[] = state.Split(';') |> Array.map int
        let initialState = State (parseState initialStatePackerd)
        let endState = State (parseState endStatePacked)

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
        let pathSets = nextSearchStep initialPaths env
        let solution = pathSets |> Seq.collect id |> Seq.filter (fun path -> path.State = endState)
        let getSolutionString (p: seq<Path>) = 
            p 
            |> Seq.truncate 1 
            |> Seq.map (fun path -> $"{path}") 
            |> String.concat "\n"
        let solution_string = getSolutionString solution

        let prop = ProvidedProperty("Property", typeof<string>, getterCode = fun args -> <@@ solution_string @@>)
        myType.AddMember(prop)

        asm.AddTypes [ myType ]

        myType

    let myParamType = 
        let t = ProvidedTypeDefinition(asm, ns, "RateProvider", Some typeof<obj>, isErased=false)
        t.DefineStaticParameters( 
            [
                ProvidedStaticParameter("initialState", typeof<string>)
                ProvidedStaticParameter("endState", typeof<string>)
            ], 
            fun typeName args -> createType typeName (unbox<string> args.[0], unbox<string> args.[1]) 
        )
        t
    do
        this.AddNamespace(ns, [myParamType])

