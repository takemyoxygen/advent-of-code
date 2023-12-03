module Utils

open System.IO

let readLines (day: int) (test: bool) =
  let inputFile =
    Path.Combine(
      __SOURCE_DIRECTORY__,
      "input",
      sprintf "day%d%s.txt" day (if test then "-test" else "")
    )

  File.ReadAllLines inputFile
