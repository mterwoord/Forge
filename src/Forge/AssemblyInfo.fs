namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Forge")>]
[<assembly: AssemblyProductAttribute("Forge")>]
[<assembly: AssemblyDescriptionAttribute("F# tool for proj and sln generation")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
