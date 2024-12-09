module GPUCompute =
    open Brahma.FSharp
    open Brahma.FSharp.OpenCL.Translator
    open System.IO

    let rateKernel (clContext: ClContext) workGroupSize =
        let kernel =
            <@
                fun (range: Range1D) (endState: ClArray<int>) (packedPerms: ClArray<int>) (ratings: ClArray<int>) ->
                    let getWrapDistance a b size =
                        min ((a - b + size) % size) ((b - a + size) % size)

                    let unpack (packedValue: int) (output: int[]) =
                        for i = 0 to 15 do
                            output.[i] <- ((packedValue >>> (i * 2)) &&& 0b11) + 1

                    let mutable total = 0
                    let id = range.GlobalID0

                    let baseIdx = id * 16
                    let packedPermutation = packedPerms.[id]

                    let perms = Array.zeroCreate 16
                    unpack packedPermutation perms


                    for i = 0 to 15 do
                        if perms.[baseIdx + i] <> endState.[i] then
                            let row = i / 4
                            let col = i % 4
                            let target = endState.[i]
                            let mutable rowShifts = 9999
                            for j = 0 to 3 do
                                if perms.[baseIdx + row * 4 + j] = target then
                                    rowShifts <- min rowShifts (getWrapDistance j col 4)
                            let mutable colShifts = 9999
                            for j = 0 to 3 do
                                if perms.[baseIdx + j * 4 + col] = target then
                                    colShifts <- min colShifts (getWrapDistance j row 4)
                            let shift =
                                match rowShifts, colShifts with
                                | 9999, 9999 -> 2
                                | rs, 9999 -> rs
                                | 9999, cs -> cs
                                | rs, cs -> min rs cs
                            total <- total + shift
                    ratings.[id] <- total
            @>

        clContext.Compile kernel

    [<EntryPoint>]
    let main argv =
        if argv.Length < 16 then
            failwith "You need to specify 16 integer command-line arguments"

        let endState =
            argv
            |> Array.map int
            |> Array.take 16

        let device = ClDevice.GetFirstAppropriateDevice()
        let context = ClContext(device)
        let mainQueue = context.QueueProvider.CreateQueue()

        let calculateUniquePermutations (arr: int array) =
            let myFactorial (n: int) = 
                let rec loop (acc: bigint) (n: int) =
                    if n <= 1 then acc
                    else loop (acc * bigint n) (n - 1)
                loop 1I n
            let counts = 
                arr 
                |> Array.countBy id
                |> Array.map snd
            let total = myFactorial (Array.sum counts)
            counts |> Array.fold (fun acc x -> acc / myFactorial x) total

        let nextPermutation (seq1: 'a array) (pred: 'a -> 'a -> int) =
            let reverse (arr: 'a array) startIdx endIdx =
                let mutable start = startIdx
                let mutable end_ = endIdx - 1
                while start < end_ do
                    let temp = arr.[start]
                    arr.[start] <- arr.[end_]
                    arr.[end_] <- temp
                    start <- start + 1
                    end_ <- end_ - 1

            if seq1.Length = 0 then
                Seq.empty
            else
                seq {
                    let seqCopy = Array.copy seq1
                    let first = 0
                    let last = seqCopy.Length

                    yield Array.copy seqCopy

                    if last > 1 then
                        let mutable loop = true
                        while loop do
                            let mutable next = last - 1
                            let mutable found = false // Control flag for breaking out of nested loop

                            while not found do
                                let next1 = next
                                next <- next - 1

                                if pred seqCopy.[next] seqCopy.[next1] < 0 then
                                    // Find the largest element to swap
                                    let mutable mid = last - 1
                                    while pred seqCopy.[next] seqCopy.[mid] >= 0 do
                                        mid <- mid - 1

                                    // Swap elements
                                    let temp = seqCopy.[next]
                                    seqCopy.[next] <- seqCopy.[mid]
                                    seqCopy.[mid] <- temp

                                    // Reverse the tail
                                    reverse seqCopy next1 last

                                    yield Array.copy seqCopy

                                    found <- true
                                elif next = first then
                                    loop <- false
                                    found <- true
                }

        //let endState = [| 4; 3; 4; 2; 3; 1; 2; 4; 4; 2; 4; 3; 2; 4; 3; 1 |]
        let endStateSorted = Array.sort endState
        let permList = nextPermutation endStateSorted compare

        let permSize = 16

        let packTo32Bit (values: int array) =
            let mutable packedValue = 0
            for i = 0 to values.Length - 1 do
                packedValue <- packedValue ||| ((values.[i] - 1) <<< (i * 2))
            packedValue

        let flatPermutations =
            let permListLength = calculateUniquePermutations endStateSorted // 25225200
            let resultArray = Array.zeroCreate (int permListLength) // 1 int for 16 elements
            permList |> Seq.iteri (fun i x ->
                resultArray.[i] <- packTo32Bit x) // pack 1 permutation to 1 int
            resultArray

        let clPermutations = context.CreateClArray(flatPermutations)
        let clEndState = context.CreateClArray(endState)
        let ratings = context.CreateClArray(Array.zeroCreate (flatPermutations.Length))

        let rateKernelFunc = rateKernel context 32

        let kernel = rateKernelFunc.GetKernel()
        let ndRange = Range1D.CreateValid(flatPermutations.Length, 32)

        mainQueue.Post(
            Msg.MsgSetArguments
                (fun () -> kernel.KernelFunc ndRange clEndState clPermutations ratings)
        )

        mainQueue.Post(Msg.CreateRunMsg<_, _> kernel)

        let hostRatings = Array.zeroCreate (flatPermutations.Length)
        let res = mainQueue.PostAndReply(fun ch -> Msg.CreateToHostMsg(ratings, hostRatings, ch))

        let endStateString = String.concat "" (Array.map string endState)
        let outputFilePath = $"ratings_{endStateString}.bin"
        use fileStream = new FileStream(outputFilePath, FileMode.Create, FileAccess.Write)
        use binaryWriter = new BinaryWriter(fileStream)
        hostRatings |> Array.iter (fun rating -> binaryWriter.Write(byte rating))

        let dictionaryFilePath = $"dictionary_{endStateString}.bin"
        use dictionaryStream = new FileStream(dictionaryFilePath, FileMode.Create, FileAccess.Write)
        use dictionaryWriter = new BinaryWriter(dictionaryStream)
        flatPermutations
        |> Array.iteri (fun index packedState ->
            dictionaryWriter.Write(packedState)
            dictionaryWriter.Write(index))
        0
