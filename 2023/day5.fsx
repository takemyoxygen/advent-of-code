#load "utils.fsx"

open System

let lines = Utils.readLines 5 false |> List.ofSeq

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

let part1 () =
  seeds |> Seq.map (mapCategories maps) |> Seq.min

// TODO: use better algorithm here, current bruteforce takes several hours to run
let part2 () =
  let mapRange start length maps =

    let result =
      seq { start .. start + length - (bigint 1) }
      |> Seq.map (mapCategories maps)
      |> Seq.min

    result


  let mapRangeAsync start length maps =
    async { return mapRange start length maps }

  seeds
  |> Array.chunkBySize 2
  |> Seq.map (fun [| start; length |] -> mapRangeAsync start length maps)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Seq.min


printfn "Day 5"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
