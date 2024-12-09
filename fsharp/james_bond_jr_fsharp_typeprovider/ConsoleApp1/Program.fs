open MyProvider2

type RateType = MyProvider2.RateProvider<
  "1; 2; 2; 1; 3; 4; 4; 3; 3; 4; 4; 3; 2; 4; 4; 2",
  "4; 3; 4; 2; 3; 1; 2; 4; 4; 2; 4; 3; 2; 4; 3; 1"
>

let rateInstance = RateType()
let prop: string = rateInstance.Property;
printfn $"{prop}"

System.Console.ReadKey() |> ignore
