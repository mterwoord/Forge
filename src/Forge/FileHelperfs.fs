(*  Based on 
        FileSystemHelper.fs from FAKE - https://github.com/fsharp/FAKE/blob/master/src/app/FakeLib/FileSystemHelper.fs
        FileHelper.fs from FAKE - https://github.com/fsharp/FAKE/blob/master/src/app/FakeLib/FileHelper.fs
*)

[<AutoOpen>]
/// Contains helper function which allow to deal with files and directories.
module Forge.FileHelper

open System
open System.IO
open System.Text
open System.Diagnostics
open System.Runtime.InteropServices
open Forge.EnvironmentHelper
open Forge.RegistryHelper
open Forge.Logging


open System.Collections.Generic
open System.Text.RegularExpressions

module Globbing =
        // Normalizes path for different OS
    let inline normalizePath (path : string) = 
        path.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar)

    type private SearchOption = 
        | Directory of string
        | Drive of string
        | Recursive
        | FilePattern of string

    let private checkSubDirs absolute (dir : string) root = 
        if dir.Contains "*" then Directory.EnumerateDirectories(root, dir, SearchOption.TopDirectoryOnly) |> Seq.toList
        else 
            let path = Path.Combine(root, dir)
        
            let di = 
                if absolute then new DirectoryInfo(dir)
                else new DirectoryInfo(path)
            if di.Exists then [ di.FullName ]
            else []

    let rec private buildPaths acc (input : SearchOption list) = 
        match input with
        | [] -> acc
        | Directory(name) :: t -> 
            let subDirs = 
                acc
                |> List.map (checkSubDirs false name)
                |> List.concat
            buildPaths subDirs t
        | Drive(name) :: t -> 
            let subDirs = 
                acc
                |> List.map (checkSubDirs true name)
                |> List.concat
            buildPaths subDirs t
        | Recursive :: [] -> 
            let dirs = 
                Seq.collect (fun dir -> Directory.EnumerateFileSystemEntries(dir, "*", SearchOption.AllDirectories)) acc 
                |> Seq.toList
            buildPaths (acc @ dirs) []
        | Recursive :: t -> 
            let dirs = 
                Seq.collect (fun dir -> Directory.EnumerateDirectories(dir, "*", SearchOption.AllDirectories)) acc 
                |> Seq.toList
            buildPaths (acc @ dirs) t
        | FilePattern(pattern) :: t -> 
             Seq.collect (fun dir -> 
                                if Directory.Exists(Path.Combine(dir, pattern))
                                then seq { yield Path.Combine(dir, pattern) }
                                else 
                                    try
                                        Directory.EnumerateFiles(dir, pattern)
                                    with
                                        | :? System.IO.PathTooLongException as ex ->
                                            Array.toSeq [| |]
                                ) acc |> Seq.toList

    let private driveRegex = Regex(@"^[A-Za-z]:$", RegexOptions.Compiled)

    let inline private normalizeOutputPath (p : string) = 
        p.Replace('\\', Path.DirectorySeparatorChar).Replace('/', Path.DirectorySeparatorChar)
         .TrimEnd(Path.DirectorySeparatorChar)

    let internal getRoot (baseDirectory : string) (pattern : string) =
        let baseDirectory = normalizePath baseDirectory
        let normPattern:string = normalizePath pattern

        let patternParts = normPattern.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
        let patternPathParts = 
            patternParts
            |> Seq.takeWhile(fun (p:string) -> not (p.Contains("*")))
            |> Seq.toArray

        let globRoot = 
            // If we did not find any "*", then drop the last bit (it is a file name, not a pattern)
            ( if patternPathParts.Length = patternParts.Length then
                  patternPathParts.[0 .. patternPathParts.Length-2]     
              else patternPathParts )
            |> String.concat (Path.DirectorySeparatorChar.ToString())

        let globRoot = 
            // If we dropped "/" from the beginning of the path in the 'Split' call, put it back!
            if normPattern.StartsWith("/") then "/" + globRoot
            else globRoot

        if Path.IsPathRooted globRoot then globRoot
        else Path.Combine(baseDirectory, globRoot)

    let internal search (baseDir : string) (input : string) = 
        let baseDir = normalizePath baseDir
        let input = normalizePath input
        let input = input.Replace(baseDir, "")

        let filePattern = Path.GetFileName(input)
        input.Split([| '/'; '\\' |], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (function 
               | "**" -> Recursive
               | a when a = filePattern -> FilePattern(a)
               | a when driveRegex.IsMatch a -> Directory(a + "\\")
               | a -> Directory(a))
        |> Seq.toList
        |> buildPaths [ baseDir ]
        |> List.map normalizeOutputPath

    let internal compileGlobToRegex pattern =
        let pattern = normalizePath pattern

        let escapedPattern = (Regex.Escape pattern)
        let regexPattern = 
            let xTOy = 
                [
                    "dirwildcard", (@"\\\*\\\*(/|\\\\)", @"(.*(/|\\))?")
                    "stardotstar", (@"\\\*\\.\\\*", @"([^\\/]*)")
                    "wildcard", (@"\\\*", @"([^\\/]*)")
                ] |> List.map(fun (key, reg) ->
                    let pattern, replace = reg
                    let pattern = sprintf "(?<%s>%s)" key pattern
                    key, (pattern, replace)
                )
            let xTOyMap = xTOy |> Map.ofList
            let replacePattern = xTOy |> List.map(fun x -> x |> snd |> fst) |> String.concat("|")
            let replaced = Regex(replacePattern).Replace(escapedPattern, fun m -> 
                let matched = xTOy |> Seq.map(fst) |> Seq.find(fun n -> 
                    m.Groups.Item(n).Success
                )
                (xTOyMap |> Map.tryFind matched).Value |> snd
            )
            "^" + replaced + "$"

        Regex(regexPattern)

    let globRegexCache = System.Collections.Concurrent.ConcurrentDictionary<string, Regex>()

    let isMatch pattern path : bool = 
        let path = normalizePath path

        let regex = 
            let outRegex : ref<Regex> = ref null
            if globRegexCache.TryGetValue(pattern, outRegex) then
                !outRegex
            else
                let compiled = compileGlobToRegex pattern
                globRegexCache.TryAdd(pattern, compiled) |> ignore
                compiled

        regex.IsMatch(path)




/// Internal representation of a file set.
type FileIncludes = 
    { BaseDirectory : string
      Includes : string list
      Excludes : string list }
    
    /// Adds the given pattern to the file includes
    member this.And pattern = { this with Includes = this.Includes @ [ pattern ] }
    
    /// Ignores files with the given pattern
    member this.ButNot pattern = { this with Excludes = pattern :: this.Excludes }
    
    /// Sets a directory as BaseDirectory.
    member this.SetBaseDirectory(dir : string) = { this with BaseDirectory = dir.TrimEnd(Path.DirectorySeparatorChar) }
    
    /// Checks if a particular file is matched
    member this.IsMatch (path : string) =
        let fullDir pattern = 
            if Path.IsPathRooted(pattern) then
                pattern
            else
                System.IO.Path.Combine(this.BaseDirectory, pattern)

        let included = 
            this.Includes
            |> Seq.exists(fun fileInclude ->
                Globbing.isMatch (fullDir fileInclude) path
            )
        let excluded = 
            this.Excludes
            |> Seq.exists(fun fileExclude ->
                Globbing.isMatch (fullDir fileExclude) path
            )

        included && not excluded

    interface IEnumerable<string> with
        
        member this.GetEnumerator() = 
            let hashSet = HashSet()
            
            let excludes = 
                seq { 
                    for pattern in this.Excludes do
                        yield! Globbing.search this.BaseDirectory pattern
                }
                |> Set.ofSeq
            
            let files = 
                seq { 
                    for pattern in this.Includes do
                        yield! Globbing.search this.BaseDirectory pattern
                }
                |> Seq.filter (fun x -> not (Set.contains x excludes))
                |> Seq.filter (fun x -> hashSet.Add x)
            
            files.GetEnumerator()
        
        member this.GetEnumerator() = (this :> IEnumerable<string>).GetEnumerator() :> System.Collections.IEnumerator

let private defaultBaseDir = Path.GetFullPath "."

/// Include files
let Include x = 
    { BaseDirectory = defaultBaseDir
      Includes = [ x ]
      Excludes = [] }

/// Sets a directory as baseDirectory for fileIncludes. 
let SetBaseDir (dir : string) (fileIncludes : FileIncludes) = fileIncludes.SetBaseDirectory dir

/// Add Include operator
let inline (++) (x : FileIncludes) pattern = x.And pattern

/// Exclude operator
let inline (--) (x : FileIncludes) pattern = x.ButNot pattern

/// Includes a single pattern and scans the files - !! x = AllFilesMatching x
let inline (!!) x = Include x

/// Looks for a tool first in its default path, if not found in all subfolders of the root folder - returns the tool file name.
let findToolInSubPath toolname defaultPath =
    try
        let tools = !! (defaultPath @@ "/**/" @@ toolname)
        if  Seq.isEmpty tools then 
            let root = !! ("./**/" @@ toolname)
            Seq.head root
        else
            Seq.head tools
    with
    | _ -> defaultPath @@ toolname

/// Looks for a tool in all subfolders - returns the folder where the tool was found.
let findToolFolderInSubPath toolname defaultPath =
    try
        let tools = !! ("./**/" @@ toolname)
        if Seq.isEmpty tools then defaultPath
        else 
            let fi = FileInfo (Seq.head tools)
            fi.Directory.FullName
    with
    | _ -> defaultPath


/// Creates a DirectoryInfo for the given path.
let inline directoryInfo path = new DirectoryInfo(path)

/// Creates a FileInfo for the given path.
let inline fileInfo path = new FileInfo(path)

/// Creates a FileInfo or a DirectoryInfo for the given path
let inline fileSystemInfo path : FileSystemInfo = 
    if Directory.Exists path then upcast directoryInfo path
    else upcast fileInfo path

/// Converts a filename to it's full file system name.
let inline FullName fileName = Path.GetFullPath fileName

/// Gets the directory part of a filename.
let inline DirectoryName fileName = Path.GetDirectoryName fileName

/// Gets all subdirectories of a given directory.
let inline subDirectories (dir : DirectoryInfo) = dir.GetDirectories()

/// Gets all files in the directory.
let inline filesInDir (dir : DirectoryInfo) = dir.GetFiles()

/// Finds all the files in the directory matching the search pattern.
let filesInDirMatching pattern (dir : DirectoryInfo) = 
    if dir.Exists then dir.GetFiles pattern
    else [||]

/// Gets the first file in the directory matching the search pattern as an option value.
let TryFindFirstMatchingFile pattern dir = 
    dir
    |> directoryInfo
    |> filesInDirMatching pattern
    |> fun files -> 
        if Seq.isEmpty files then None
        else (Seq.head files).FullName |> Some

/// Gets the first file in the directory matching the search pattern or throws an error if nothing was found.
let FindFirstMatchingFile pattern dir = 
    match TryFindFirstMatchingFile pattern dir with
    | Some x -> x
    | None -> new FileNotFoundException(sprintf "Could not find file matching %s in %s" pattern dir) |> raise

/// Gets the current directory.
let currentDirectory = Path.GetFullPath "."

/// Get the full location of the current assembly.
let fullAssemblyPath = System.Reflection.Assembly.GetAssembly(typeof<RegistryBaseKey>).Location

/// Checks if the file exists on disk.
let fileExists fileName = File.Exists fileName

/// Raises an exception if the file doesn't exist on disk.
let checkFileExists fileName = 
    if not <| fileExists fileName then new FileNotFoundException(sprintf "File %s does not exist." fileName) |> raise

/// Checks if all given files exist.
let allFilesExist files = Seq.forall fileExists files

/// Normalizes a filename.
let rec normalizeFileName (fileName : string) = 
    fileName.Replace("\\", Path.DirectorySeparatorChar.ToString()).Replace("/", Path.DirectorySeparatorChar.ToString())
            .TrimEnd(Path.DirectorySeparatorChar).ToLower()

/// Checks if dir1 is a subfolder of dir2. If dir1 equals dir2 the function returns also true.
let rec isSubfolderOf (dir2 : DirectoryInfo) (dir1 : DirectoryInfo) = 
    if normalizeFileName dir1.FullName = normalizeFileName dir2.FullName then true
    else if dir1.Parent = null then false
    else dir1.Parent |> isSubfolderOf dir2

/// Checks if the file is in a subfolder of the dir.
let isInFolder (dir : DirectoryInfo) (fileInfo : FileInfo) = isSubfolderOf dir fileInfo.Directory

/// Checks if the directory exists on disk.
let directoryExists dir = Directory.Exists dir

/// Ensure that directory chain exists. Create necessary directories if necessary.
let inline ensureDirExists (dir : DirectoryInfo) = 
    if not dir.Exists then dir.Create()

/// Checks if the given directory exists. If not then this functions creates the directory.
let inline ensureDirectory dir = directoryInfo dir |> ensureDirExists

/// Detects whether the given path is a directory.
let isDirectory path = 
    let attr = File.GetAttributes path
    attr &&& FileAttributes.Directory = FileAttributes.Directory

/// Detects whether the given path is a file.
let isFile path = isDirectory path |> not

/// Detects whether the given path does not contains invalid characters.
let isValidPath (path:string) =
    Path.GetInvalidPathChars()
    |> Array.filter (fun char -> path.Contains(char.ToString()))
    |> Array.isEmpty


/// Performs the given actions on all files and subdirectories
let rec recursively dirF fileF (dir : DirectoryInfo) = 
    dir
    |> subDirectories
    |> Seq.iter (fun dir -> 
           recursively dirF fileF dir
           dirF dir)
    dir
    |> filesInDir
    |> Seq.iter fileF

/// Sets the directory readonly 
let setDirectoryReadOnly readOnly (dir : DirectoryInfo) = 
    if dir.Exists then 
        let isReadOnly = dir.Attributes &&& FileAttributes.ReadOnly = FileAttributes.ReadOnly
        if readOnly && (not isReadOnly) then dir.Attributes <- dir.Attributes ||| FileAttributes.ReadOnly
        if (not readOnly) && not isReadOnly then dir.Attributes <- dir.Attributes &&& (~~~FileAttributes.ReadOnly)

/// Sets all files in the directory readonly.
let SetDirReadOnly readOnly dir = 
    recursively (setDirectoryReadOnly readOnly) (fun file -> file.IsReadOnly <- readOnly) dir

/// Sets all given files readonly.
let SetReadOnly readOnly (files : string seq) = 
    files |> Seq.iter (fun file -> 
                 let fi = fileInfo file
                 if fi.Exists then fi.IsReadOnly <- readOnly
                 else 
                     file
                     |> directoryInfo
                     |> setDirectoryReadOnly readOnly)

/// Deletes a directory if it exists.
let DeleteDir path = 
    let dir = directoryInfo path
    if dir.Exists then 
        // set all files readonly = false
        !!"/**/*.*"
        |> SetBaseDir dir.FullName
        |> (SetReadOnly false)
        logfn "Deleting %s" dir.FullName
        dir.Delete true
    else logfn "%s does not exist." dir.FullName

/// Creates a directory if it does not exist.
let CreateDir path = 
    let dir = directoryInfo path
    if not dir.Exists then 
        logfn "Creating %s" dir.FullName
        dir.Create()
    else logfn "%s already exists." dir.FullName

/// Creates a file if it does not exist.
let CreateFile fileName = 
    let file = fileInfo fileName
    if not file.Exists then 
        logfn "Creating %s" file.FullName
        let newFile = file.Create()
        newFile.Close()
    else logfn "%s already exists." file.FullName

/// Deletes a file if it exists.
let DeleteFile fileName = 
    let file = fileInfo fileName
    if file.Exists then 
        logfn "Deleting %s" file.FullName
        file.Delete()
    else logfn "%s does not exist." file.FullName

/// Deletes the given files.
let DeleteFiles files = Seq.iter DeleteFile files

/// Active pattern which discriminates between files and directories.
let (|File|Directory|) (fileSysInfo : FileSystemInfo) = 
    match fileSysInfo with
    | :? FileInfo as file -> File(file)
    | :? DirectoryInfo as dir -> Directory(dir, dir.EnumerateFileSystemInfos())
    | _ -> failwith "No file or directory given."

/// Active Pattern for determining file extension.
let (|EndsWith|_|) extension (file : string) = 
    if file.EndsWith extension then Some()
    else None

/// Active Pattern for determining file name.
let (|FileInfoFullName|) (f : FileInfo) = f.FullName

/// Active Pattern for determining FileInfoNameSections.
let (|FileInfoNameSections|) (f : FileInfo) = (f.Name, f.Extension, f.FullName)

/// Copies a single file to the target and overwrites the existing file.
/// ## Parameters
/// 
///  - `target` - The target directory or file.
///  - `fileName` - The FileName.
let CopyFile target fileName = 
    let fi = fileSystemInfo fileName
    match fi with
    | File f -> 
        let targetName = 
            match fileSystemInfo target with
            | Directory _ -> target @@ fi.Name
            | File f' -> f'.FullName
        logVerbosefn "Copy %s to %s" fileName targetName
        f.CopyTo(targetName, true) |> ignore
    | Directory _ -> logVerbosefn "Ignoring %s, because it is a directory." fileName

let private DoCopyFile targetName fileName =
    let fi = fileInfo fileName
    let target = fileInfo targetName
    ensureDirExists target.Directory
    logVerbosefn "Copy %s to %s" fileName targetName
    fi.CopyTo(targetName, true) |> ignore

/// Copies a single file to a relative subfolder of the target.
/// ## Parameters
///
///  - `target` - The target directory
///  - `fileName` - The fileName
let CopyFileIntoSubFolder target fileName =
    let relative = (toRelativePath fileName).TrimStart '.'
    DoCopyFile (target + relative) fileName

/// Copies a single file to the target folder preserving the folder structure
/// starting from the specified base folder.
/// ## Parameters
///
///  - `baseDir` - The base directory.
///  - `target` - The target directory.
///  - `fileName` - The file name.
let CopyFileWithSubfolder baseDir target fileName =
    let fileName = FullName fileName
    let baseDir = FullName baseDir
    let relative = (ProduceRelativePath baseDir fileName).TrimStart '.'
    DoCopyFile (target + relative) fileName

/// Copies several file groups, each represented by a FileIncludes object,
/// to the target folder preserving the folder structure
/// starting from the BaseDirectory of each FileIncludes.
/// ## Parameters
///
///  - `target` - The target directory.
///  - `files` - A sequence of file groups.
let CopyWithSubfoldersTo target files =
    let copyFiles dir inc = Seq.iter (CopyFileWithSubfolder dir target) inc
    Seq.iter (fun inc -> copyFiles inc.BaseDirectory inc) files

/// Copies the files to the target.
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `files` - The original file names as a sequence.
let Copy target files = 
    ensureDirectory target
    files |> Seq.iter (CopyFile target)

/// Copies the given files to the target.
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `files` - The original file names as a sequence.
let CopyTo target files = Copy target files

/// Copies the files from a cache folder.
/// If the files are not cached or the original files have a different write time the cache will be refreshed.
/// ## Parameters
/// 
///  - `target` - The target FileName.
///  - `cacheDir` - The cache directory.
///  - `files` - The orginal files.
let CopyCached target cacheDir files = 
    let cache = directoryInfo cacheDir
    ensureDirExists cache
    files
    |> Seq.map (fun fileName -> 
           let fi = fileInfo fileName
           let cached = cacheDir @@ fi.Name
           let cachedFi = fileInfo cached
           
           let originalExists = 
               try 
                   fi.Exists
               with exn -> false
           if not originalExists then 
               if not cachedFi.Exists then failwithf "Original file %s and cached file %s do not exist." fileName cached
               else tracefn "Original file %s does not exist, using cached file %s." fileName cached
           else if not cachedFi.Exists || cachedFi.LastWriteTime <> fi.LastWriteTime then 
               tracefn "Cached file %s doesn't exist or is not up to date. Copying file to cache." cached
               CopyFile cacheDir fi.FullName
           else tracefn "Cached file %s is up to date." cached
           CopyFile target cached
           target @@ fi.Name)
    |> Seq.toList

/// Renames the file to the target file name.
/// ## Parameters
/// 
///  - `target` - The target file name.
///  - `file` - The orginal file name.
let Rename target fileName = (fileInfo fileName).MoveTo target

/// Copies a list of files to the specified directory without any output.
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `files` - List of files to copy.
let SilentCopy target files = 
    files |> Seq.iter (fun file -> 
                 let fi = fileInfo file
                 let targetName = target @@ fi.Name
                 let targetFI = fileInfo targetName
                 if targetFI.Exists then 
                     if fi.LastWriteTime > targetFI.LastWriteTime then 
                         targetFI.Attributes <- FileAttributes.Normal
                         fi.CopyTo(targetName, true) |> ignore
                 else fi.CopyTo(targetName) |> ignore)

/// Copies the files to the target - Alias for Copy
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `files` - The orginal file names.
let CopyFiles target files = Copy target files

/// Exclude SVN files (path with .svn)
let excludeSVNFiles (path : string) = not <| path.Contains ".svn"

/// Includes all files
let allFiles (path : string) = true

/// Copies a directory recursivly. If the target directory does not exist, it will be created.
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `source` - The source directory.
///  - `filterFile` - A file filter predicate.
let CopyDir target source filterFile = 
    CreateDir target
    Directory.GetFiles(source, "*.*", SearchOption.AllDirectories)
    |> Seq.filter filterFile
    |> Seq.iter (fun file -> 
           let fi = 
               file
               |> replaceFirst source ""
               |> trimSeparator
           
           let newFile = target @@ fi
           logVerbosefn "%s => %s" file newFile
           DirectoryName newFile |> ensureDirectory
           File.Copy(file, newFile, true))
    |> ignore

/// Cleans a directory by removing all files and sub-directories.
let CleanDir path = 
    let di = directoryInfo path
    if di.Exists then 
        logfn "Deleting contents of %s" path
        // delete all files
        Directory.GetFiles(path, "*.*", SearchOption.AllDirectories) |> Seq.iter (fun file -> 
                                                                            let fi = fileInfo file
                                                                            fi.IsReadOnly <- false
                                                                            fi.Delete())
        // deletes all subdirectories
        let rec deleteDirs actDir = 
            Directory.GetDirectories(actDir) |> Seq.iter deleteDirs
            Directory.Delete(actDir, true)
        Directory.GetDirectories path |> Seq.iter deleteDirs
    else CreateDir path
    // set writeable
    File.SetAttributes(path, FileAttributes.Normal)

/// Cleans multiple directories
let CleanDirs dirs = Seq.iter CleanDir dirs

/// Deletes multiple directories
let DeleteDirs dirs = Seq.iter DeleteDir dirs

/// Reads a csv file line by line
/// delimiter is a ,
let ReadCSVFile(file : string) = 
    let csvRegEx = new RegularExpressions.Regex(",(?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))")
    ReadFile file
    |> Seq.map csvRegEx.Split
    |> Seq.map (Array.map (fun s -> s.Trim [| '"' |]))

/// Appends all given files to one file.
/// ## Parameters
/// 
///  - `newFileName` - The target FileName.
///  - `files` - The original FileNames as a sequence.
let AppendTextFiles newFileName files = 
    let fi = fileInfo newFileName
    if fi.Exists then failwithf "File %s already exists." (fi.FullName)
    use writer = new StreamWriter(fi.FullName, false, encoding)
    files |> Seq.iter (fun file -> 
                 logVerbosefn "Appending %s to %s" file fi.FullName
                 ReadFile file |> Seq.iter writer.WriteLine)

/// Checks if the two files are byte-to-byte equal.
let FilesAreEqual (first : FileInfo) (second : FileInfo) = 
    if first.Length <> second.Length then false
    else 
        let BYTES_TO_READ = 32768
        use fs1 = first.OpenRead()
        use fs2 = second.OpenRead()
        let one = Array.create BYTES_TO_READ (byte 0)
        let two = Array.create BYTES_TO_READ (byte 0)
        let mutable eq = true
        while eq && fs1.Read(one, 0, BYTES_TO_READ) <> 0 && fs2.Read(two, 0, BYTES_TO_READ) <> 0 do
            if one <> two then eq <- false
        eq

/// Compares the given files for changes.
/// If delete is set to true then equal files will be removed.
let CompareFiles delete originalFileName compareFileName = 
    let ori = fileInfo originalFileName
    let comp = fileInfo compareFileName
    
    let identical = 
        if not (ori.Exists && comp.Exists && ori.Length = comp.Length) then false
        else ori.LastWriteTime = comp.LastWriteTime || FilesAreEqual ori comp
    if not identical then false
    else 
        if delete then 
            comp.Attributes <- FileAttributes.Normal
            comp.Delete()
            logVerbosefn "Deleting File: %s" comp.FullName
        else logVerbosefn "Files equal: %s" comp.FullName
        true

/// Checks if the directory exists
let TestDir path = 
    let di = directoryInfo path
    if di.Exists then true
    else 
        logfn "%s not found" di.FullName
        false
        
/// Checks if the file exists
let TestFile path = 
    let fi = fileInfo path
    if fi.Exists then true
    else 
        logfn "%s not found" fi.FullName
        false

/// Checks the srcFiles for changes to the last release.
/// ## Parameters
/// 
///  - `lastReleaseDir` - The directory of the last release
///  - `patchDir` - The target directory
///  - `srcFiles` - The source files
///  - `findOldFileF` - A function which finds the old file
let GeneratePatchWithFindOldFileFunction lastReleaseDir patchDir srcFiles findOldFileF = 
    let i = ref 0
    for file in srcFiles do
        let newFile = toRelativePath file
        let oldFile = findOldFileF newFile (lastReleaseDir + newFile.TrimStart('.'))
        let fi = fileInfo oldFile
        if not fi.Exists then logVerbosefn "LastRelease has no file like %s" fi.FullName
        if CompareFiles false oldFile newFile |> not then 
            i := !i + 1
            CopyFileIntoSubFolder patchDir newFile
    tracefn "Patch contains %d files." !i

/// Checks the srcFiles for changes to the last release.
/// ## Parameters
/// 
///  - `lastReleaseDir` - The directory of the last release.
///  - `patchDir` - The target directory.
///  - `srcFiles` - The source files.
let GeneratePatch lastReleaseDir patchDir srcFiles = 
    GeneratePatchWithFindOldFileFunction lastReleaseDir patchDir srcFiles (fun a b -> b)

/// Copies the file structure recursively.
let rec copyRecursive (dir : DirectoryInfo) (outputDir : DirectoryInfo) overwrite = 
    let files = 
        dir
        |> subDirectories
        |> Seq.fold (fun acc (d : DirectoryInfo) -> 
               let newDir = outputDir.FullName @@ d.Name
                            |> directoryInfo
               if not newDir.Exists then newDir.Create()
               copyRecursive d newDir overwrite @ acc) []
    (dir
     |> filesInDir
     |> Seq.map (fun f -> 
            let newFileName = outputDir.FullName @@ f.Name
            f.CopyTo(newFileName, overwrite) |> ignore
            newFileName)
     |> Seq.toList) @ files

/// Copies the file structure recursively.
let CopyRecursive dir outputDir = copyRecursive (directoryInfo dir) (directoryInfo outputDir)

/// Moves a single file to the target and overwrites the existing file.
/// ## Parameters
/// 
///  - `target` - The target directory.
///  - `fileName` - The FileName.
let MoveFile target fileName = 
    let fi = fileSystemInfo fileName
    match fi with
    | File f -> 
        let targetName = target @@ fi.Name
        let targetInfo = fileInfo targetName
        if targetInfo.Exists then targetInfo.Delete()
        logVerbosefn "Move %s to %s" fileName targetName
        f.MoveTo(targetName) |> ignore
    | Directory _ -> logVerbosefn "Ignoring %s, because it is a directory." fileName

/// Creates a config file with the parameters as "key;value" lines
let WriteConfigFile configFileName parameters = 
    if isNullOrEmpty configFileName then ()
    else 
        let fi = fileInfo configFileName
        if fi.Exists then fi.Delete()
        use streamWriter = fi.CreateText()
        for (key, value) in parameters do
            streamWriter.WriteLine("{0};{1}", key, value)

/// Replaces all occurences of the patterns in the given files with the given replacements.
/// ## Parameters
///
///  - `replacements` - A sequence of tuples with the patterns and the replacements.
///  - `files` - The files to process.
let ReplaceInFiles replacements files = processTemplates replacements files

/// Replace all occurences of the regex pattern with the given replacement in the specified file
/// ## Parameters
///
/// - `pattern` - The string to search for a match
/// - `replacement` - The replacement string
/// - `encoding` - The encoding to use when reading and writing the file
/// - `file` - The path of the file to process
let RegexReplaceInFileWithEncoding pattern (replacement:string) encoding file =
    let oldContent = File.ReadAllText(file, encoding)
    let newContent = System.Text.RegularExpressions.Regex.Replace(oldContent, pattern, replacement)
    File.WriteAllText(file, newContent, encoding)

/// Replace all occurences of the regex pattern with the given replacement in the specified files
/// ## Parameters
///
/// - `pattern` - The string to search for a match
/// - `replacement` - The replacement string
/// - `encoding` - The encoding to use when reading and writing the files
/// - `files` - The paths of the files to process
let RegexReplaceInFilesWithEncoding pattern (replacement:string) encoding files =
    files |> Seq.iter (RegexReplaceInFileWithEncoding pattern replacement encoding)

/// Get the version a file.
/// ## Parameters
///
///  - 'fileName' - Name of file from which the version is retrieved. The path can be relative.
let FileVersion(fileName : string) = 
    FullName fileName
    |> FileVersionInfo.GetVersionInfo
    |> fun x -> x.FileVersion.ToString()

/// Get the filename extension including the leading '.', or an empty string if the file has no extension.
/// ## Parameters
///
/// - 'fileName' - Name of the file from which the extension is retrieved.
let ext fileName = Path.GetExtension fileName
/// Change the extension of the file.
/// ## Parameters
///
/// - 'extension' - The new extension containing the leading '.'.
/// - 'fileName' - Name of the file from which the extension is retrieved.
let changeExt extension fileName = Path.ChangeExtension(fileName, extension)

/// Tests whether the file has specified extensions (containing the leading '.')
/// ## Parameters
///
/// - 'extension' - The extension to fine containing the leading '.'.
/// - 'fileName' - Name of the file from which the extension is retrieved.
let hasExt extension fileName = System.String.Equals(ext fileName, extension, System.StringComparison.InvariantCultureIgnoreCase)

/// Get the filename for the specified path
/// ## Parameters
///
/// - 'path' - The path from which the filename is retrieved.
let filename path = Path.GetFileName path

/// Get the filename for the specified path without it's extension
/// ## Parameters
///
/// - 'path' - The path from which the filename is retrieved.
let fileNameWithoutExt path = Path.GetFileNameWithoutExtension path

