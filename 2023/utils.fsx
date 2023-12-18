module Utils

module Input =
  open System.IO

  let readLines (day: int) (test: bool) =
    let inputFile =
      Path.Combine(
        __SOURCE_DIRECTORY__,
        "input",
        sprintf "day%d%s.txt" day (if test then "-test" else "")
      )

    File.ReadAllLines inputFile

module Seq =
  let all pred xs = xs |> Seq.exists (pred >> not) |> not

module String =
  let tryGet idx (s: string) =
    if 0 <= idx && idx < s.Length then Some(s[idx]) else None

module List =
  let repeat n xs =
    xs |> List.replicate n |> List.collect id
