module Forge.Requirements

open System
open Forge
open Forge.Domain

[<RequireQualifiedAccess>]
type FrameworkRestriction = 
| Exactly of FrameworkIdentifier
| Portable of string
| AtLeast of FrameworkIdentifier
| Between of FrameworkIdentifier * FrameworkIdentifier
    
    override this.ToString() =
        match this with
        | FrameworkRestriction.Exactly r -> r.ToString()
        | FrameworkRestriction.Portable r -> r
        | FrameworkRestriction.AtLeast r -> ">= " + r.ToString()
        | FrameworkRestriction.Between(min,max) -> sprintf ">= %O < %O" min max

    member private x.GetOneIdentifier =
        match x with
        | Exactly r -> Some r
        | Portable _ -> None
        | AtLeast r -> Some r
        | Between(r, _) -> Some r

    /// Return if the parameter is a restriction of the same framework category (dotnet, windows phone, silverlight, ...)
    member x.IsSameCategoryAs (y : FrameworkRestriction) =
        match (x.GetOneIdentifier, y.GetOneIdentifier) with
        | Some r, Some r' -> Some(r.IsSameCategoryAs r')
        | _ -> None

type FrameworkRestrictions = FrameworkRestriction list



let parseRestrictions(text:string) =
    let text =
        // workaround missing spaces
        text.Replace("<=","<= ").Replace(">=",">= ").Replace("=","= ")

    let commaSplit = text.Trim().Split(',')
    [for p in commaSplit do
        let operatorSplit = p.Trim().Split([|' '|],StringSplitOptions.RemoveEmptyEntries)
        let framework =
            if operatorSplit.Length < 2 then 
                operatorSplit.[0] 
            else 
                operatorSplit.[1]


        match FrameworkDetection.Extract(framework) with
        | None -> 
                if PlatformMatching.extractPlatforms framework |> Array.isEmpty |> not then
                    yield FrameworkRestriction.Portable framework
        | Some x -> 
            if operatorSplit.[0] = ">=" then
                if operatorSplit.Length < 4 then
                    yield FrameworkRestriction.AtLeast x
                else
                    match FrameworkDetection.Extract(operatorSplit.[3]) with
                    | None -> ()
                    | Some y -> yield FrameworkRestriction.Between(x,y)
            else
                yield FrameworkRestriction.Exactly x]

let private minRestriction = FrameworkRestriction.Exactly(DotNetFramework(FrameworkVersion.V1))

let findMaxDotNetRestriction restrictions =
    minRestriction :: restrictions
    |> List.filter (fun (r:FrameworkRestriction) ->
        match r with
        | FrameworkRestriction.Exactly r -> r.ToString().StartsWith("net")
        | _ -> false)
    |> List.max
    |> fun r ->
        match r with
        | FrameworkRestriction.Exactly r -> r
        | _ -> failwith "error"

let rec optimizeRestrictions restrictions = 
    let sorting xs =
        xs
        |> List.sortBy (fun x ->
            match x with
            | FrameworkRestriction.Exactly r -> r
            | FrameworkRestriction.Portable r -> FrameworkIdentifier.MonoMac
            | FrameworkRestriction.AtLeast r -> r
            | FrameworkRestriction.Between(min,max) -> min
            |> fun y -> y,x)
            
    match sorting restrictions |> List.distinct with
    | [] -> []
    | [x] -> [x]
    | odered ->
        let newRestrictions' =
            match odered |> Seq.tryFind (function | FrameworkRestriction.AtLeast r -> true | _ -> false) with
            | Some((FrameworkRestriction.AtLeast(DotNetFramework(v)) as r)) ->
                odered
                |> List.filter (fun r' ->
                    match r' with
                    | FrameworkRestriction.Exactly(DotNetFramework(x)) when x > v -> false
                    | FrameworkRestriction.AtLeast(DotNetFramework(x)) when x > v -> false
                    | _ -> true)
            | _ -> odered

        let newRestrictions =
            match newRestrictions' |> Seq.rev |> Seq.tryFind (function | FrameworkRestriction.AtLeast r -> true | _ -> false) with
            | None -> newRestrictions'
            | Some r ->
                let currentVersion =
                    match r with
                    | FrameworkRestriction.AtLeast(DotNetFramework(x)) -> x
                    | x -> failwithf "Unknown .NET moniker %O" x
                                                                                                           
                let isLowerVersion currentVersion x =
                    let isMatching x =
                        if x = FrameworkVersion.V3_5 && currentVersion = FrameworkVersion.V4 then true else
                        if x = FrameworkVersion.V3_5 && currentVersion = FrameworkVersion.V4_Client then true else
                        if x = FrameworkVersion.V4_Client && currentVersion = FrameworkVersion.V4_5 then true else
                        let hasFrameworksBetween = KnownTargetProfiles.DotNetFrameworkVersions |> Seq.exists (fun p -> p > x && p < currentVersion)
                        not hasFrameworksBetween

                    match x with
                    | FrameworkRestriction.Exactly(DotNetFramework(x)) -> isMatching x
                    | FrameworkRestriction.AtLeast(DotNetFramework(x)) -> isMatching x
                    | _ -> false

                match newRestrictions' |> Seq.tryFind (isLowerVersion currentVersion) with
                | None -> newRestrictions'
                | Some n -> 
                    let newLowest =
                        match n with
                        | FrameworkRestriction.Exactly(DotNetFramework(x)) -> x
                        | FrameworkRestriction.AtLeast(DotNetFramework(x)) -> x
                        | x -> failwithf "Unknown .NET moniker %O" x

                    let filtered =
                        newRestrictions'
                        |> List.filter (fun x -> x <> r && x <> n)

                    filtered @ [FrameworkRestriction.AtLeast(DotNetFramework(newLowest))]
                                        
        if restrictions = newRestrictions then sorting newRestrictions else optimizeRestrictions newRestrictions



let private combineSameCategoryOrPortableRestrictions x y =
    match x with
    | FrameworkRestriction.Exactly r -> 
        match y with
        | FrameworkRestriction.Exactly r' -> if r = r' then [FrameworkRestriction.Exactly r] else []
        | FrameworkRestriction.Portable _ -> []
        | FrameworkRestriction.AtLeast r' -> if r' <= r then [FrameworkRestriction.Exactly r] else []
        | FrameworkRestriction.Between(min,max) -> if min <= r && r < max then [FrameworkRestriction.Exactly r] else []
    | FrameworkRestriction.Portable r ->
        match y with
        | FrameworkRestriction.Portable r' -> if r = r' then [FrameworkRestriction.Portable r] else []
        | _ -> []
    | FrameworkRestriction.AtLeast r ->
        match y with
        | FrameworkRestriction.Exactly r' -> if r <= r' then [FrameworkRestriction.Exactly r'] else []
        | FrameworkRestriction.Portable _ -> []
        | FrameworkRestriction.AtLeast r' -> [FrameworkRestriction.AtLeast (max r r')]
        | FrameworkRestriction.Between(min,max') -> if r < max' then [FrameworkRestriction.Between(max r min,max')] else []
    | FrameworkRestriction.Between(min1,max1) ->
        match y with
        | FrameworkRestriction.Exactly r -> if min1 <= r && r < max1 then [FrameworkRestriction.Exactly r] else []
        | FrameworkRestriction.Portable _ -> []
        | FrameworkRestriction.AtLeast r -> if r < max1 then [FrameworkRestriction.Between(max r min1,max1)] else []
        | FrameworkRestriction.Between(min2,max2) -> 
            let min' = max min1 min2
            let max' = min max1 max2
            if min' < max' then [FrameworkRestriction.Between(min',max')] else
            if min' = max' then [FrameworkRestriction.Exactly(min')] else
            []

let combineRestrictions (x : FrameworkRestriction) y =
    if (x.IsSameCategoryAs(y) = Some(false)) then
        []
    else
        combineSameCategoryOrPortableRestrictions x y

let filterRestrictions (list1:FrameworkRestrictions) (list2:FrameworkRestrictions) =
    match list1,list2 with
    | [],_ -> list2
    | _,[] -> list1
    | _ ->
        [for x in list1 do
            for y in list2 do
                let c = combineRestrictions x y
                if c <> [] then yield! c]
    |> optimizeRestrictions

/// Get if a target should be considered with the specified restrictions
let isTargetMatchingRestrictions (restrictions:FrameworkRestrictions) = function
    | SinglePlatform pf ->
        restrictions
        |> List.exists (fun restriction ->
                match restriction with
                | FrameworkRestriction.Exactly fw -> pf = fw
                | FrameworkRestriction.Portable _ -> false
                | FrameworkRestriction.AtLeast fw -> pf >= fw && pf.IsSameCategoryAs(fw)
                | FrameworkRestriction.Between(min,max) -> pf >= min && pf < max && pf.IsSameCategoryAs(min))
    | _ ->
        restrictions
        |> List.exists (fun restriction ->
                match restriction with
                | FrameworkRestriction.Portable r -> true
                | _ -> false)

/// Get all targets that should be considered with the specified restrictions
let applyRestrictionsToTargets (restrictions:FrameworkRestrictions) (targets: TargetProfile list) =
    let result = targets |> List.filter (isTargetMatchingRestrictions restrictions)
    result


type ContentCopySettings =
| Omit
| Overwrite
| OmitIfExisting

type BindingRedirectsSettings =
| On
| Off
| Force

type InstallSettings = 
    { ImportTargets : bool option
      FrameworkRestrictions: FrameworkRestrictions
      OmitContent : ContentCopySettings option
      IncludeVersionInPath: bool option
      ReferenceCondition : string option
      CreateBindingRedirects : BindingRedirectsSettings option
      CopyLocal : bool option }

    static member Default =
        { CopyLocal = None
          ImportTargets = None
          FrameworkRestrictions = []
          IncludeVersionInPath = None
          ReferenceCondition = None
          CreateBindingRedirects = None
          OmitContent = None }

    member this.ToString(asLines) =
        let options =
            [ match this.CopyLocal with
              | Some x -> yield "copy_local: " + x.ToString().ToLower()
              | None -> ()
              match this.ImportTargets with
              | Some x -> yield "import_targets: " + x.ToString().ToLower()
              | None -> ()
              match this.OmitContent with
              | Some ContentCopySettings.Omit -> yield "content: none"
              | Some ContentCopySettings.Overwrite -> yield "content: true"
              | Some ContentCopySettings.OmitIfExisting -> yield "content: once"
              | None -> ()
              match this.IncludeVersionInPath with
              | Some x -> yield "version_in_path: " + x.ToString().ToLower()
              | None -> ()
              match this.ReferenceCondition with
              | Some x -> yield "condition: " + x.ToUpper()
              | None -> ()
              match this.CreateBindingRedirects with
              | Some On -> yield "redirects: on"
              | Some Off -> yield "redirects: off"
              | Some Force -> yield "redirects: force"
              | None -> ()
              match this.FrameworkRestrictions with
              | [] -> ()
              | _  -> yield "framework: " + (String.Join(", ",this.FrameworkRestrictions))]

        let separator = if asLines then Environment.NewLine else ", "
        String.Join(separator,options)

    override this.ToString() = this.ToString(false)

//    static member (+)(self, other : InstallSettings) =
//        {
//            self with 
//                ImportTargets = self.ImportTargets ++ other.ImportTargets
//                FrameworkRestrictions = filterRestrictions self.FrameworkRestrictions other.FrameworkRestrictions
//                OmitContent = self.OmitContent ++ other.OmitContent
//                CopyLocal = self.CopyLocal ++ other.CopyLocal
//                ReferenceCondition = self.ReferenceCondition ++ other.ReferenceCondition
//                IncludeVersionInPath = self.IncludeVersionInPath ++ other.IncludeVersionInPath
//        }

    static member Parse(text:string) : InstallSettings =
        let kvPairs = parseKeyValuePairs text

        { ImportTargets =
            match kvPairs.TryGetValue "import_targets" with
            | true, "false" -> Some false 
            | true, "true" -> Some true
            | _ -> None
          FrameworkRestrictions =
            match kvPairs.TryGetValue "framework" with
            | true, s -> parseRestrictions s
            | _ -> []
          OmitContent =
            match kvPairs.TryGetValue "content" with
            | true, "none" -> Some ContentCopySettings.Omit 
            | true, "once" -> Some ContentCopySettings.OmitIfExisting
            | true, "true" -> Some ContentCopySettings.Overwrite
            | _ ->  None
          CreateBindingRedirects =
            match kvPairs.TryGetValue "redirects" with
            | true, "on" -> Some On 
            | true, "off" -> Some Off
            | true, "force" -> Some Force
            | _ ->  None
          IncludeVersionInPath =
            match kvPairs.TryGetValue "version_in_path" with
            | true, "false" -> Some false 
            | true, "true" -> Some true
            | _ -> None 
          ReferenceCondition =
            match kvPairs.TryGetValue "condition" with
            | true, c -> Some(c.ToUpper())
            | _ -> None 
          CopyLocal =
            match kvPairs.TryGetValue "copy_local" with
            | true, "false" -> Some false 
            | true, "true" -> Some true
            | _ -> None }

//    member this.AdjustWithSpecialCases(packageName) =
//        if packageName = PackageName "Microsoft.Bcl.Build" && this.ImportTargets = None then
//            // Microsoft.Bcl.Build targets file causes the build to fail in VS
//            // so users have to be very explicit with the targets file
//            { this with ImportTargets = Some false }
//        else
//            this


type RemoteFileInstallSettings = 
    { Link : bool option }

    static member Default =
        { Link = None }

    member this.ToString(asLines) =
        let options =
            [ match this.Link with
              | Some x -> yield "link: " + x.ToString().ToLower()
              | None -> ()]

        let separator = if asLines then Environment.NewLine else ", "
        String.Join(separator,options)

    override this.ToString() = this.ToString(false)

    static member Parse(text:string) : RemoteFileInstallSettings =
        let kvPairs = parseKeyValuePairs text

        { Link =
            match kvPairs.TryGetValue "link" with
            | true, "false" -> Some false 
            | true, "true" -> Some true
            | _ -> None }

