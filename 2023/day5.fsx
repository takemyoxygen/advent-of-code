#load "utils.fsx"

open Utils

let lines = Input.readLines 5 false |> List.ofSeq

let seeds, maps =
  let seedsLine :: _ :: rest = lines

  let seeds =
    seedsLine.Replace("seeds: ", "").Split(" ") |> Array.map bigint.Parse

  let consumeMap lines =
    let rec loop acc lines =
      match lines with
      | [] -> acc, lines
      | "" :: rest -> acc, rest
      | line :: rest ->
        let [| destStart; sourceStart; length |] =
          line.Split(" ") |> Array.map bigint.Parse

        loop ((sourceStart, destStart, length) :: acc) rest

    loop List.empty lines


  let rec readMaps acc lines =
    match lines with
    | [] -> acc
    | "" :: rest -> readMaps acc rest
    | header :: rest when header.EndsWith("map:") ->
      let map, rest = consumeMap rest
      readMaps ((map |> List.sortBy (fun (x, _, _) -> x)) :: acc) rest
    | _ -> failwithf "Unexpected line: %s" <| List.head lines

  let maps = readMaps List.empty rest |> List.rev

  seeds, maps

let part1 () =
  let mapCategory x map =
    let rec loop mappings =
      match mappings with
      | [] -> x
      | (src, _, _) :: _ when x < src -> x
      | (src, dest, length) :: _ when src <= x && x < src + length ->
        dest + (x - src)
      | _ :: rest -> loop rest

    loop map

  let mapCategories maps x = List.fold mapCategory x maps


  seeds |> Seq.map (mapCategories maps) |> Seq.min

let part2 () =
  let maps =
    maps
    |> List.map (
      List.map (fun (start, dest, length) -> (start, start + length - 1I, dest))
    )

  let seedRanges =
    seeds
    |> Array.chunkBySize 2
    |> Seq.map (fun [| start; length |] -> start, start + length - 1I)
    |> List.ofSeq

  let project mStart dest start finish =
    let diff = start - mStart
    let size = finish - start
    let projectedStart = dest + diff
    projectedStart, projectedStart + size

  let mapInterval start finish mappings =
    let rec loop start finish mappings results =
      match mappings with
      | _ when start > finish -> results
      | [] -> (start, finish) :: results
      | (mStart, _, _) :: _ when finish < mStart -> (start, finish) :: results
      | (_, mFinish, _) :: rest when start > mFinish ->
        loop start finish rest results
      | (mStart, _, _) :: _ when start < mStart && mStart <= finish ->
        (start, mStart - 1I) :: results |> loop mStart finish mappings
      | (mStart, mFinish, dest) :: rest ->
        let toMapStart, toMapFinish = start, min finish mFinish

        (project mStart dest toMapStart toMapFinish) :: results
        |> loop (toMapFinish + 1I) finish rest

    loop start finish mappings List.empty |> List.rev

  let mapAll start finish =
    Seq.fold
      (fun results mapping ->
        List.collect (fun (st, fin) -> mapInterval st fin mapping) results)
      [ start, finish ]
      maps


  seedRanges
  |> Seq.collect (fun (st, fin) -> mapAll st fin)
  |> Seq.map fst
  |> Seq.min


printfn "Day 5"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
