open System
open System.IO

let day = Environment.GetCommandLineArgs() |> Array.last |> Int32.Parse

printfn "Creating files for day %d..."
let sourceFile = Path.Combine(__SOURCE_DIRECTORY__, $"day{day}.fsx")

if File.Exists sourceFile then
  printfn "File %s already exists" sourceFile
else
  let content =
    [| "#load \"utils.fsx\""
       ""
       "open Utils"
       ""
       $"let input = Input.readLines {day} true" |]

  File.WriteAllLines(sourceFile, content)
  printfn "Created %s" sourceFile


[ Path.Combine(__SOURCE_DIRECTORY__, "input", $"day{day}.txt")
  Path.Combine(__SOURCE_DIRECTORY__, "input", $"day{day}-test.txt") ]
|> Seq.filter (File.Exists >> not)
|> Seq.iter (fun path ->
  File.Create(path).Dispose()
  printfn "Created %s" path)
