#load "utils.fsx"

open Utils

let lines = Input.readLines 11 false

let expandCoord offsetStep size getCoord expandCoord galaxies =
  seq { 0I .. size - 1I }
  |> Seq.fold
    (fun (acc, offset) i ->
      let currentCoordGalaxies = galaxies |> Set.filter (getCoord >> ((=) i))

      if Set.isEmpty currentCoordGalaxies then
        acc, offset + offsetStep
      else
        Set.map (expandCoord offset) currentCoordGalaxies |> Set.union acc,
        offset)
    (Set.empty, 0I)
  |> fst

let expandLines offsetStep =
  expandCoord offsetStep (bigint lines.Length) snd (fun offset (x, y) ->
    x, y + offset)

let expandColumns offsetStep =
  expandCoord offsetStep (bigint lines[0].Length) fst (fun offset (x, y) ->
    x + offset, y)

let galaxies =
  seq {
    for y in 0 .. lines.Length - 1 do
      for x in 0 .. lines[y].Length - 1 do
        if lines[y][x] = '#' then
          yield x, y
  }
  |> Seq.map (fun (x, y) -> (bigint x), (bigint y))
  |> set

let part1 () =
  galaxies
  |> expandLines 1I
  |> expandColumns 1I
  |> (fun g -> Seq.allPairs g g)
  |> Seq.sumBy (fun ((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2))
  |> (fun res -> res / 2I)

let part2 () =
  galaxies
  |> expandLines (1000000I - 1I)
  |> expandColumns (1000000I - 1I)
  |> (fun g -> Seq.allPairs g g)
  |> Seq.sumBy (fun ((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2))
  |> (fun res -> res / 2I)

printfn "Day 11"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
