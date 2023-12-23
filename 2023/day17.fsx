#load "utils.fsx"
#r "nuget: FSharpx.Collections"

open System
open FSharpx.Collections
open Utils

let grid =
  Input.readLines 17 false
  |> Array.map (fun line ->
    line.ToCharArray() |> Array.map (fun c -> Int32.Parse(c.ToString())))
  |> Grid.create

let next canTurn canMoveInSameDir grid pos dir stepsInDir =
  seq {
    if canMoveInSameDir stepsInDir then
      yield Direction.move pos dir, dir, stepsInDir + 1

    if canTurn stepsInDir then
      yield!
        [ Direction.clockwise dir; Direction.counterclockwise dir ]
        |> Seq.map (fun dir' -> Direction.move pos dir', dir', 1)
  }
  |> Seq.filter (fun (pos, _, _) -> Grid.isWithin pos grid)


let findPath next grid (shouldStop: GridPos -> int -> bool) =
  let rec loop visited heap =
    match heap with
    | Heap.Nil -> failwith "Destination is unreachable!"
    | Heap.Cons((path, pos, _, stepsInDir), _) when shouldStop pos stepsInDir ->
      path
    | Heap.Cons((_, pos, dir, stepsInDir), heap') when
      Set.contains (pos, dir, stepsInDir) visited
      ->
      loop visited heap'
    | Heap.Cons((path, pos, dir, stepsInDir), heap') ->
      next grid pos dir stepsInDir
      |> Seq.fold
        (fun h (pos, dir, steps) ->
          Heap.insert (path + Grid.at pos grid, pos, dir, steps) h)
        heap'
      |> loop (Set.add (pos, dir, stepsInDir) visited)

  loop Set.empty (Heap.ofSeq false [ 0, { Row = 0; Col = 0 }, Right, 0 ])


let part1 () =
  let dst =
    { Row = Grid.rowsCount grid - 1
      Col = Grid.columnsCount grid - 1 }

  let next = next (fun _ -> true) ((>) 3)
  findPath next grid (fun pos _ -> dst = pos)

let part2 () =
  let dst =
    { Row = Grid.rowsCount grid - 1
      Col = Grid.columnsCount grid - 1 }

  let next = next ((<=) 4) ((>) 10)
  findPath next grid (fun pos stepsInDir -> pos = dst && stepsInDir >= 4)


printfn "Day 17"
printfn "Part 1: %A" <| part1 ()
printfn "Part 2: %A" <| part2 ()
