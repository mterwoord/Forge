namespace Forge
open Forge
open Forge.Domain
open Forge.EnvironmentHelper


[<AutoOpen>]
module Pervasive =

    open System
    open System.IO
    open System.Xml
    open System.Xml.Linq
    open Forge
    open Logging

    let inline force (lz: 'a Lazy)  = lz.Force()
    let inline endsWith text x = (^a:(member EndsWith:string->bool)x, text) 
    let inline toLower str = (^a:(member ToLower:unit->string)str) 
    let inline trim str = (^a:(member Trim:unit->string)str) 


        // MonadPlus - "or else"
    let inline (++) x y =
        match x with
        | None -> y
        | _ -> x

    let parseKeyValuePairs (s:string) =
        let s = s.Trim().ToLower()
        let parts = s.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
        let dict = new System.Collections.Generic.Dictionary<_,_>()

        let lastKey = ref ""

        for p in parts do
            if p.Contains ":" then
                let innerParts = p.Split ':' |> Array.map trim
                if innerParts.Length % 2 <> 0 then
                    failwithf "\"%s\" can not be parsed as key/value pairs." p
                dict.Add(innerParts.[0],innerParts.[1])
                lastKey := innerParts.[0]
            else
                dict.[!lastKey] <- dict.[!lastKey] + ", " + p
        dict


    /// Represents the result of a computation.
    type Result<'TSuccess, 'TMessage> = 
        /// Represents the result of a successful computation.
        | Ok of 'TSuccess * 'TMessage list
        /// Represents the result of a failed computation.
        | Bad of 'TMessage list

        /// Creates a Failure result with the given messages.
        static member FailWith(messages:'TMessage seq) : Result<'TSuccess, 'TMessage> = Result<'TSuccess, 'TMessage>.Bad(messages |> Seq.toList)

        /// Creates a Failure result with the given message.
        static member FailWith(message:'TMessage) : Result<'TSuccess, 'TMessage> = Result<'TSuccess, 'TMessage>.Bad([message])
    
        /// Creates a Success result with the given value.
        static member Succeed(value:'TSuccess) : Result<'TSuccess, 'TMessage> = Result<'TSuccess, 'TMessage>.Ok(value,[])

        /// Creates a Success result with the given value and the given message.
        static member Succeed(value:'TSuccess,message:'TMessage) : Result<'TSuccess, 'TMessage> = Result<'TSuccess, 'TMessage>.Ok(value,[message])

        /// Creates a Success result with the given value and the given message.
        static member Succeed(value:'TSuccess,messages:'TMessage seq) : Result<'TSuccess, 'TMessage> = Result<'TSuccess, 'TMessage>.Ok(value,messages |> Seq.toList)

        /// Executes the given function on a given success or captures the failure
        static member Try(func: Func<_>) : Result<'TSuccess,exn> =        
            try
                Ok(func.Invoke(),[])
            with
            | exn -> Bad[exn]

        /// Converts the result into a string.
        override this.ToString() =
            match this with
            | Ok(v,msgs) -> sprintf "OK: %A - %s" v (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))
            | Bad(msgs) -> sprintf "Error: %s" (String.Join(Environment.NewLine, msgs |> Seq.map (fun x -> x.ToString())))  


    /// Wraps a value in a Success
    let inline ok<'TSuccess,'TMessage> (x:'TSuccess) : Result<'TSuccess,'TMessage> = Ok(x, [])

    /// Wraps a value in a Success
    let inline pass<'TSuccess,'TMessage> (x:'TSuccess) : Result<'TSuccess,'TMessage> = Ok(x, [])

    /// Wraps a value in a Success and adds a message
    let inline warn<'TSuccess,'TMessage> (msg:'TMessage) (x:'TSuccess) : Result<'TSuccess,'TMessage> = Ok(x,[msg])

    /// Wraps a message in a Failure
    let inline fail<'TSuccess,'Message> (msg:'Message) : Result<'TSuccess,'Message> = Bad([ msg ])

    /// Returns true if the result was not successful.
    let inline failed result = 
        match result with
        | Bad _ -> true
        | _ -> false

    /// Takes a Result and maps it with fSuccess if it is a Success otherwise it maps it with fFailure.
    let inline either fSuccess fFailure trialResult = 
        match trialResult with
        | Ok(x, msgs) -> fSuccess (x, msgs)
        | Bad(msgs) -> fFailure (msgs)

    /// If the given result is a Success the wrapped value will be returned. 
    ///Otherwise the function throws an exception with Failure message of the result.
    let inline returnOrFail result = 
        let inline raiseExn msgs = 
            msgs
            |> Seq.map (sprintf "%O")
            |> String.concat (Environment.NewLine + "\t")
            |> failwith
        either fst raiseExn result



    open System.Diagnostics

    /// Maybe computation expression builder, copied from ExtCore library
    /// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
    [<Sealed>]
    type MaybeBuilder () =
        // 'T -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Return value: 'T option = Some value

        // M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.ReturnFrom value: 'T option = value

        // unit -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Zero (): unit option = Some ()     // TODO: Should this be None?

        // (unit -> M<'T>) -> M<'T>
        [<DebuggerStepThrough>]
        member __.Delay (f: unit -> 'T option): 'T option = f ()

        // M<'T> -> M<'T> -> M<'T>
        // or
        // M<unit> -> M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member inline __.Combine (r1, r2: 'T option): 'T option =
            match r1 with
            | None    -> None
            | Some () -> r2

        // M<'T> * ('T -> M<'U>) -> M<'U>
        [<DebuggerStepThrough>]
        member inline __.Bind (value, f: 'T -> 'U option): 'U option = Option.bind f value

        // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
        [<DebuggerStepThrough>]
        member __.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
            try body resource
            finally
                if not <| obj.ReferenceEquals (null, box resource) then
                    resource.Dispose ()

        // (unit -> bool) * M<'T> -> M<'T>
        [<DebuggerStepThrough>]
        member x.While (guard, body: _ option): _ option =
            if guard () then
                // OPTIMIZE: This could be simplified so we don't need to make calls to  While.
                Option.bind (fun () -> x.While (guard, body)) body
            else Some ()  // x.Zero ()
            
            
        // seq<'T> * ('T -> M<'U>) -> M<'U>
        // or
        // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
        [<DebuggerStepThrough>]
        member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
            // OPTIMIZE: This could be simplified so we don't need to make calls to While
            using (sequence.GetEnumerator ())(fun enum ->
                x.While (enum.MoveNext, body enum.Current))

    let maybe = MaybeBuilder() 


    let inline xelem  name (content:'a)  = new XElement (XName.Get name, content)

    let inline addAttribute name value (node:XmlElement) =
        node.SetAttribute(name, value) |> ignore
        node

    let inline addChild child (node:XmlElement) =
        node.AppendChild(child) |> ignore
        node

    let inline hasAttribute name (node:XmlNode) =
        if isNull node || isNull node.Attributes then false else
        node.Attributes 
        |> Seq.cast<XmlAttribute>
        |> Seq.exists (fun a -> a.Name = name)

    let inline getAttribute name (node:XmlNode) =
        if isNull node || isNull node.Attributes then None else
        node.Attributes 
        |> Seq.cast<XmlAttribute> 
        |> Seq.tryFind (fun a -> a.Name = name && (isNull a.Value |> not)) 
        |> Option.map (fun a -> a.Value)

    let inline withAttributeValue attributeName valueText node =
        getAttribute attributeName node = Some valueText

    let inline optGetAttribute name node = node |> Option.bind (getAttribute name)

    let inline getNode name (node:XmlNode) =
        let xpath = sprintf "*[local-name() = '%s']" name
        match node.SelectSingleNode(xpath) with
        | null -> None
        | n -> Some(n)

    let inline optGetNode name node = node |> Option.bind (getNode name)

    let inline getNodes name (node:XmlNode) =
        let xpath = sprintf "*[local-name() = '%s']" name
        match node.SelectNodes(xpath) with
        | null -> []
        | nodeList -> 
            nodeList
            |> Seq.cast<XmlNode>
            |> Seq.toList

    let inline getDescendants name (node:XmlNode) = 
        let xpath = sprintf ".//*[local-name() = '%s']" name
        match node.SelectNodes(xpath) with
        | null -> []
        | nodeList -> 
            nodeList
            |> Seq.cast<XmlNode>
            |> Seq.toList

    let inline getChildNodes (node:XmlNode) = System.Linq.Enumerable.Cast<XmlNode>(node)

    type XContainer with
        member this.Element     name            = this.Element(XName.Get name)
        member this.HasElement (name:string)    = not (isNull (this.Element name))
        member this.AddElement  name            = this.Add (xelem name []); this.Element name

        member this.GetOrCreateElement name =
            if this.HasElement name then this.Element name else this.AddElement name

        member this.SetOrCreateElement name value =
            if this.HasElement name then (this.Element name).Value <- value
            else (this.AddElement name).Value <- value

    let acceptXml = "application/atom+xml,application/xml"
    let acceptJson = "application/atom+json,application/json"

    let notNullOrEmpty = not << System.String.IsNullOrEmpty

    type Auth = 
        | Credentials of Username : string * Password : string
        | Token of string

    let TimeSpanToReadableString(span:TimeSpan) =
        let pluralize x = if x = 1 then String.Empty else "s"
        let notZero x y = if x > 0 then y else String.Empty
        let days = notZero (span.Duration().Days)  <| String.Format("{0:0} day{1}, ", span.Days, pluralize span.Days)
        let hours = notZero (span.Duration().Hours) <| String.Format("{0:0} hour{1}, ", span.Hours, pluralize span.Hours) 
        let minutes = notZero (span.Duration().Minutes) <| String.Format("{0:0} minute{1}, ", span.Minutes, pluralize span.Minutes)
        let seconds = notZero (span.Duration().Seconds) <| String.Format("{0:0} second{1}", span.Seconds, pluralize span.Seconds) 

        let formatted = String.Format("{0}{1}{2}{3}", days, hours, minutes, seconds)

        let formatted = if formatted.EndsWith ", " then formatted.Substring(0, formatted.Length - 2) else formatted

        if String.IsNullOrEmpty formatted then "0 seconds" else formatted

    let GetHomeDirectory() =
        if  Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX then
            Environment.GetEnvironmentVariable "HOME"
        else
            Environment.ExpandEnvironmentVariables "%HOMEDRIVE%%HOMEPATH%"

    type PathReference =
        | AbsolutePath of string
        | RelativePath of string

    let normalizeLocalPath (path:string) =
        if path.StartsWith "~/" then
            AbsolutePath (Path.Combine(GetHomeDirectory(), path.Substring 2))
        elif Path.IsPathRooted path then
            AbsolutePath path
        else
            RelativePath path
        
    let getDirectoryInfo pathInfo root =
        match pathInfo with
        | AbsolutePath s -> DirectoryInfo s 
        | RelativePath s -> DirectoryInfo(Path.Combine(root, s))
        
    /// Creates a directory if it does not exist.
    let createDir path = 
        try
            let dir = DirectoryInfo path
            if not dir.Exists then dir.Create()
            ok ()
        with _ ->
            DirectoryCreateError path |> fail

    let rec deleteDir (dirInfo:DirectoryInfo) =
        if dirInfo.Exists then
            for fileInfo in dirInfo.GetFiles() do
                fileInfo.Attributes <- FileAttributes.Normal
                fileInfo.Delete()

            for childInfo in dirInfo.GetDirectories() do
                deleteDir childInfo

            dirInfo.Attributes <- FileAttributes.Normal
            dirInfo.Delete()

    /// Cleans a directory by deleting it and recreating it.
    let CleanDir path = 
        let di = DirectoryInfo path
        if di.Exists then 
            try
                deleteDir di
            with
            | exn -> failwithf "Error during deletion of %s%s  - %s" di.FullName Environment.NewLine exn.Message 
        di.Create()
        // set writeable
        File.SetAttributes (path, FileAttributes.Normal)

    open System.Text

    // http://stackoverflow.com/a/19283954/1397724
    let getFileEncoding path =
        let bom = Array.zeroCreate 4
        use fs = new FileStream (path, FileMode.Open, FileAccess.Read)
        fs.Read (bom, 0, 4) |> ignore
        match bom with
        | [| 0x2buy ; 0x2fuy ; 0x76uy ; _      |] -> Encoding.UTF7
        | [| 0xefuy ; 0xbbuy ; 0xbfuy ; _      |] -> Encoding.UTF8
        | [| 0xffuy ; 0xfeuy ; _      ; _      |] -> Encoding.Unicode //UTF-16LE
        | [| 0xfeuy ; 0xffuy ; _      ; _      |] -> Encoding.BigEndianUnicode //UTF-16BE
        | [| 0uy    ; 0uy    ; 0xfeuy ; 0xffuy |] -> Encoding.UTF32
        | _ -> Encoding.ASCII

    /// [omit]
    let inline createRelativePath root path = 
        let basePath = 
            if String.IsNullOrEmpty root then Environment.CurrentDirectory + string Path.DirectorySeparatorChar
            else root
    
        let uri = Uri basePath
        uri.MakeRelativeUri(Uri path).ToString().Replace("/", "\\").Replace("%20", " ")

    let extractPath infix (fileName : string) : string option=
        let path = fileName.Replace("\\", "/").ToLower()
        let startPos = path.LastIndexOf (sprintf "%s/" infix)
        let endPos = path.IndexOf('/', startPos + infix.Length + 1)
        if startPos < 0 then None 
        elif endPos < 0 then Some ""
        else 
            Some (path.Substring(startPos + infix.Length + 1, endPos - startPos - infix.Length - 1))

    /// [omit]
    let inline normalizeXml (doc:XmlDocument) =
        use stringWriter = new StringWriter()
        let settings = XmlWriterSettings (Indent=true)
        
        use xmlTextWriter = XmlWriter.Create (stringWriter, settings)
        doc.WriteTo xmlTextWriter
        xmlTextWriter.Flush()
        stringWriter.GetStringBuilder() |> string




    open System.Diagnostics
    open System.Threading

    let innerText (exn:Exception) =
        match exn.InnerException with
        | null -> ""
        | exn -> Environment.NewLine + " Details: " + exn.Message


    let readAnswer() = System.Console.ReadLine().Trim()



    let inline normalizePath(path:string) = path.Replace("\\",Path.DirectorySeparatorChar.ToString()).Replace("/",Path.DirectorySeparatorChar.ToString()).TrimEnd(Path.DirectorySeparatorChar)

    /// Gets all files with the given pattern
    let inline FindAllFiles(folder, pattern) = DirectoryInfo(folder).GetFiles(pattern, SearchOption.AllDirectories)

[<AutoOpen>]
module StringHelper = 
    open System
    open System.IO
    open System.Text
    open System.Collections.Generic
    open Forge.EnvironmentHelper

    /// Returns if the string is null or empty
    let inline isNullOrEmpty value = String.IsNullOrEmpty value

    /// Returns if the string is not null or empty
    let inline isNotNullOrEmpty value = String.IsNullOrEmpty value |> not

    /// Returns if the string is null or empty or completely whitespace
    let inline isNullOrWhiteSpace value = isNullOrEmpty value || value |> Seq.forall Char.IsWhiteSpace

    /// Replaces the given pattern in the given text with the replacement
    let inline replace (pattern : string) replacement (text : string) = text.Replace(pattern, replacement)

    /// Converts a sequence of strings to a string with delimiters
    let inline separated delimiter (items : string seq) = String.Join(delimiter, Array.ofSeq items)

    /// Removes the slashes from the end of the given string
    let inline trimSlash (s : string) = s.TrimEnd('\\')

    /// Splits the given string at the given char delimiter
    let inline split (delimiter : char) (text : string) = text.Split [| delimiter |] |> Array.toList

    /// Splits the given string at the given string delimiter
    let inline splitStr (delimiterStr : string) (text : string) = 
        text.Split([| delimiterStr |], StringSplitOptions.None) |> Array.toList

    /// Converts a sequence of strings into a string separated with line ends
    let inline toLines text = separated Environment.NewLine text

    /// Checks whether the given text starts with the given prefix
    let startsWith prefix (text : string) = text.StartsWith prefix

    /// Checks whether the given text ends with the given suffix
    let endsWith suffix (text : string) = text.EndsWith suffix

    /// Determines whether the last character of the given <see cref="string" />
    /// matches Path.DirectorySeparatorChar.         
    let endsWithSlash = endsWith (Path.DirectorySeparatorChar.ToString())

    /// Replaces the first occurrence of the pattern with the given replacement.
    let replaceFirst (pattern : string) replacement (text : string) = 
        let pos = text.IndexOf pattern
        if pos < 0 then text
        else text.Remove(pos, pattern.Length).Insert(pos, replacement)

    /// Appends a text to a StringBuilder.
    let inline append text (builder : StringBuilder) = builder.Append(sprintf "\"%s\" " text)

    /// Appends a text to a StringBuilder without surrounding quotes.
    let inline appendWithoutQuotes (text : string) (builder : StringBuilder) = builder.Append(sprintf "%s " text)

    /// Appends string of function value if option has some value
    let inline appendIfSome o f builder = 
        match o with
        | Some(value) -> appendWithoutQuotes (f value) builder
        | None -> builder

    /// Appends a text if the predicate is true.
    let inline appendIfTrue p s builder = 
        if p then append s builder
        else builder

    let inline appendIfTrueWithoutQuotes p s builder = 
        if p then appendWithoutQuotes s builder
        else builder

    /// Appends a text if the predicate is false.
    let inline appendIfFalse p = appendIfTrue (not p)

    /// Appends a text without quoting if the value is not null.
    let inline appendWithoutQuotesIfNotNull (value : Object) s = 
        appendIfTrueWithoutQuotes 
            (value <> null) 
            (match value with
             | :? String as sv -> (sprintf "%s%s" s sv)
             | _ -> (sprintf "%s%A" s value))

    /// Appends a text if the value is not null.
    let inline appendIfNotNull (value : Object) s = 
        appendIfTrue 
            (value <> null) 
            (match value with
             | :? String as sv -> (sprintf "%s%s" s sv)
             | _ -> (sprintf "%s%A" s value))

    /// Appends a quoted text if the value is not null.
    let inline appendQuotedIfNotNull (value : Object) s (builder : StringBuilder) = 
        if (value = null) then builder
        else 
            (match value with
             | :? String as sv -> builder.Append(sprintf "%s\"%s\" " s sv)
             | _ -> builder.Append(sprintf "%s\"%A\" " s value))

    /// Appends a text if the value is not null.
    let inline appendStringIfValueIsNotNull value = appendIfTrue (value <> null)

    /// Appends a text if the value is not null or empty.
    let inline appendStringIfValueIsNotNullOrEmpty value = appendIfTrue (isNullOrEmpty value |> not)

    /// Appends a text if the value is not null or empty.
    let inline appendIfNotNullOrEmpty value s = appendIfTrue (isNotNullOrEmpty value) (sprintf "%s%s" s value)

    /// Appends all notnull fileNames.
    let inline appendFileNamesIfNotNull fileNames (builder : StringBuilder) = 
        fileNames |> Seq.fold (fun builder file -> appendIfTrue (isNullOrEmpty file |> not) file builder) builder

    /// Returns the text from the StringBuilder
    let inline toText (builder : StringBuilder) = builder.ToString()

    /// [omit]
    let private regexes = new Dictionary<_, _>()

    /// [omit]
    let getRegEx pattern = 
        match regexes.TryGetValue pattern with
        | true, regex -> regex
        | _ -> (new System.Text.RegularExpressions.Regex(pattern))

    /// [omit]
    let regex_replace pattern (replacement : string) text = (getRegEx pattern).Replace(text, replacement)

    /// Checks whether the given char is a german umlaut.
    let isUmlaut c = Seq.exists ((=) c) [ 'ä'; 'ö'; 'ü'; 'Ä'; 'Ö'; 'Ü'; 'ß' ]

    /// Converts all characters in a string to lower case.
    let inline toLower (s : string) = s.ToLower()

    /// Returns all standard chars and digits.
    let charsAndDigits = [ 'a'..'z' ] @ [ 'A'..'Z' ] @ [ '0'..'9' ]

    /// Checks whether the given char is a standard char or digit.
    let isLetterOrDigit c = List.exists ((=) c) charsAndDigits

    /// Trims the given string with the DirectorySeparatorChar
    let inline trimSeparator (s : string) = s.TrimEnd Path.DirectorySeparatorChar

    /// Trims all special characters from a string.
    let inline trimSpecialChars (text : string) = 
        text
        |> Seq.filter isLetterOrDigit
        |> Seq.filter (isUmlaut >> not)
        |> Seq.fold (fun (acc : string) c -> acc + string c) ""

    /// Trims the given string
    let inline trim (x : string) = 
        if isNullOrEmpty x then x
        else x.Trim()

    /// Trims the given string
    let inline trimChars chars (x : string) = 
        if isNullOrEmpty x then x
        else x.Trim chars

    /// Trims the start of the given string
    let inline trimStartChars chars (x : string) =
        if isNullOrEmpty x then x
        else x.TrimStart chars

    /// Trims the end of the given string
    let inline trimEndChars chars (x : string) =
        if isNullOrEmpty x then x
        else x.TrimEnd chars

    /// Lifts a string to an option
    let liftString x = 
        if isNullOrEmpty x then None
        else Some x

    /// Reads a file line by line
    let ReadFile(file : string) = 
        seq { 
            use textReader = new StreamReader(file, encoding)
            while not textReader.EndOfStream do
                yield textReader.ReadLine()
        }

    /// Reads the first line of a file. This can be helpful to read a password from file.
    let ReadLine(file : string) = 
        use sr = new StreamReader(file, Encoding.Default)
        sr.ReadLine()

    /// Creates a FileInfo for the given path.
    let inline fileInfo path = new FileInfo(path)


    /// Writes a file line by line
    let WriteToFile append fileName (lines : seq<string>) = 
        let fi = fileInfo fileName
        use writer = new StreamWriter(fileName, append && fi.Exists, encoding)
        lines |> Seq.iter writer.WriteLine

    /// Removes all trailing .0 from a version string
    let rec NormalizeVersion(version : string) =
        if version = null then "" else
        let elements = version.Split [| '.' |]
        let mutable version = ""
        for i in 0..3 do
            if i < elements.Length then 
                if version = "" then version <- elements.[i]
                else version <- version + "." + elements.[i]
        if version.EndsWith ".0" then version.Remove(version.Length - 2, 2) |> NormalizeVersion
        else version

    /// Writes a byte array to a file
    let WriteBytesToFile file bytes = File.WriteAllBytes(file, bytes)

    /// Writes a string to a file
    let WriteStringToFile append fileName (text : string) = 
        let fi = fileInfo fileName
        use writer = new StreamWriter(fileName, append && fi.Exists, encoding)
        writer.Write text

    /// Replaces the file with the given string
    let ReplaceFile fileName text = 
        let fi = fileInfo fileName
        if fi.Exists then 
            fi.IsReadOnly <- false
            fi.Delete()
        WriteStringToFile false fileName text

    let Colon = ','

    /// Writes a file line by line
    let WriteFile file lines = WriteToFile false file lines

    /// Appends all lines to a file line by line
    let AppendToFile file lines = WriteToFile true file lines

    /// Reads a file as one text
    let inline ReadFileAsString file = File.ReadAllText(file, encoding)

    /// Reads a file as one array of bytes
    let ReadFileAsBytes file = File.ReadAllBytes file


    /// Gets the current directory.
    let currentDirectory = Path.GetFullPath "."

    /// Replaces any occurence of the currentDirectory with .
    let inline shortenCurrentDirectory value = replace currentDirectory "." value

    /// Checks whether the given text starts with the given prefix
    let inline (<*) prefix text = startsWith prefix text

    /// Replaces the text in the given file
    let ReplaceInFile replaceF fileName = 
        fileName
        |> ReadFileAsString
        |> replaceF
        |> ReplaceFile fileName

    /// Represents Linux line breaks
    let LinuxLineBreaks = "\n"

    /// Represents Windows line breaks
    let WindowsLineBreaks = "\r\n"

    /// Represents Mac line breaks
    let MacLineBreaks = "\r"

    /// Converts all line breaks in a text to windows line breaks
    let ConvertTextToWindowsLineBreaks text = 
        text
        |> replace WindowsLineBreaks LinuxLineBreaks
        |> replace MacLineBreaks LinuxLineBreaks
        |> replace LinuxLineBreaks WindowsLineBreaks

    /// Reads a file line by line and replaces all line breaks to windows line breaks
    ///   - uses a temp file to store the contents in order to prevent OutOfMemory exceptions
    let ConvertFileToWindowsLineBreaks(fileName : string) = 
        use reader = new StreamReader(fileName, encoding)
        let tempFileName = Path.GetTempFileName()
        use writer = new StreamWriter(tempFileName, false, encoding)
        while not reader.EndOfStream do
            reader.ReadLine()
            |> ConvertTextToWindowsLineBreaks
            |> writer.WriteLine
        reader.Close()
        writer.Close()
        File.Delete(fileName)
        File.Move(tempFileName, fileName)

    /// Removes linebreaks from the given string
    let inline RemoveLineBreaks text = 
        text
        |> replace "\r" String.Empty
        |> replace "\n" String.Empty

    /// Encapsulates the Apostrophe
    let inline EncapsulateApostrophe text = replace "'" "`" text

    /// A cache of relative path names.
    /// [omit]
    let relativePaths = new Dictionary<_, _>()

    /// <summary>Produces relative path when possible to go from baseLocation to targetLocation.</summary>
    /// <param name="baseLocation">The root folder</param>
    /// <param name="targetLocation">The target folder</param>
    /// <returns>The relative path relative to baseLocation</returns>
    /// <exception cref="ArgumentNullException">base or target locations are null or empty</exception>
    let ProduceRelativePath baseLocation targetLocation = 
        if isNullOrEmpty baseLocation then raise (new ArgumentNullException "baseLocation")
        if isNullOrEmpty targetLocation then raise (new ArgumentNullException "targetLocation")
        if not <| Path.IsPathRooted baseLocation then baseLocation
        else if not <| Path.IsPathRooted targetLocation then targetLocation
        else if String.Compare(Path.GetPathRoot baseLocation, Path.GetPathRoot targetLocation, true) <> 0 then 
            targetLocation
        else if String.Compare(baseLocation, targetLocation, true) = 0 then "."
        else 
            let resultPath = ref "."
        
            let targetLocation = 
                if targetLocation |> endsWith directorySeparator then targetLocation
                else targetLocation + directorySeparator
        
            let baseLocation = 
                if baseLocation |> endsWith directorySeparator then ref (baseLocation.Substring(0, baseLocation.Length - 1))
                else ref baseLocation
        
            while not <| targetLocation.StartsWith(!baseLocation + directorySeparator, StringComparison.OrdinalIgnoreCase) do
                resultPath := !resultPath + directorySeparator + ".."
                baseLocation := Path.GetDirectoryName !baseLocation
                if (!baseLocation) |> endsWith directorySeparator then 
                    baseLocation := (!baseLocation).Substring(0, (!baseLocation).Length - 1)
            resultPath 
            := (!resultPath + targetLocation.Substring((!baseLocation).Length)) 
               |> replace (directorySeparator + directorySeparator) directorySeparator
            // preprocess .\..\ case
            if (sprintf ".%s..%s" directorySeparator directorySeparator) <* (!resultPath) then 
                (!resultPath).Substring(2, (!resultPath).Length - 3)
            else (!resultPath).Substring(0, (!resultPath).Length - 1)

    /// Replaces the absolute path to a relative path.
    let inline toRelativePath value = 
        match relativePaths.TryGetValue value with
        | true, x -> x
        | _ -> 
            let x = ProduceRelativePath currentDirectory value
            relativePaths.Add(value, x)
            x

    /// Find a regex pattern in a text and replaces it with the given replacement.
    let (>=>) pattern replacement text = regex_replace pattern replacement text

    /// Determines if a text matches a given regex pattern.
    let (>**) pattern text = (getRegEx pattern).IsMatch text

    /// Decodes a Base64-encoded UTF-8-encoded string
    let DecodeBase64Utf8String(text : string) = 
        text
        |> Convert.FromBase64String
        |> Encoding.UTF8.GetString


[<AutoOpen>]
/// Contains basic templating functions. Used in other helpers.
module TemplateHelper =

    /// Loads all templates (lazy - line by line!)    
    let loadTemplates seq = Seq.map (fun fileName -> fileName, ReadFile fileName) seq

    /// Replaces a bunch of the keywords in all files (lazy - line by line!)
    let replaceKeywords replacements = 
        Seq.map (fun (fileName, file) -> 
            fileName, 
            file |> Seq.map (fun (line : string) -> 
                        let mutable sb = new System.Text.StringBuilder(line)
                        for (k : string, r : string) in replacements do
                            sb <- sb.Replace(k, r)
                        sb.ToString()))

    /// Saves all files (lazy - file by file!)
    let saveFiles = Seq.iter (fun (fileName, file) -> WriteFile fileName (Seq.toList file))

    /// Replaces the templates with the given replacements
    let processTemplates replacements files = 
        files
        |> loadTemplates
        |> replaceKeywords replacements
        |> saveFiles
