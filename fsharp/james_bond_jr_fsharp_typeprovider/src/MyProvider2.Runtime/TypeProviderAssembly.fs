namespace MyNamespace

open FSharp.Core.CompilerServices

// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly: TypeProviderAssembly("MyProvider2.DesignTime")>]
do ()